{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.PeerManager where

import Butter.Core.MetaInfo (InfoHash)
import Butter.Core.Peer (Peer(..), PeerEvent, startPeer)
import Butter.Core.PeerWire
import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import qualified Data.Map.Strict as Map (Map, empty, insert)
import Data.Conduit.Network (sourceSocket)
import Network (PortID(..), listenOn)
import Network.Socket (Socket, SockAddr(..), PortNumber(..), accept, close)

type PeerHandler = Socket -> SockAddr -> IO ()

data PeerManager = PeerManager { pmPeerId         :: PeerId
                               , pmPort           :: PortNumber
                               , pmInfoHash       :: InfoHash
                               , pmMapVar         :: TVar (Map.Map PeerId Peer)
                               , pmPeerEventsChan :: Chan PeerEvent
                               }

newPeerManager :: PeerId -> PortNumber -> InfoHash -> IO PeerManager
newPeerManager peerId port infoHash = do
    sock            <- listenOn $ Network.PortNumber port
    eventsChan      <- newChan
    pmapVar         <- newTVarIO Map.empty

    let m = PeerManager peerId port infoHash pmapVar eventsChan

    _ <- forkIO $ handleEvents eventsChan pmapVar
    _ <- forkIO $ acceptPeers sock (handlePeer m)

    return m

acceptPeers :: Socket -> PeerHandler -> IO ()
acceptPeers sock handler = forever $ do
    (peersock, addr) <- accept sock
    forkFinally (handler peersock addr) (\_ -> close sock)

handleEvents :: Chan PeerEvent -> TVar (Map.Map PeerId Peer) -> IO ()
handleEvents ch pmapVar = consumeChannel ch (handleEvent pmapVar)

handleEvent :: TVar (Map.Map PeerId Peer) -> PeerEvent -> IO ()
handleEvent = undefined

addFromChan :: PeerManager -> Chan PeerAddr -> IO ()
addFromChan m pchan = forever $ do
    peerAddr <- readChan pchan
    esock <- try $ connectToPeer peerAddr :: IO (Either SomeException Socket)
    case esock of
        Left _ -> return ()
        Right sock -> do
            sendMessage sock PWHandshake { pwHandshakePeerId = pmPeerId m
                                         , pwHandshakeInfoHash = pmInfoHash m
                                         }
            peer <- startPeer (pmPeerEventsChan m) sock Nothing
            atomically $ modifyTVar (pmMapVar m) (Map.insert (pId peer) peer)

handlePeer :: PeerManager -> Socket -> SockAddr -> IO ()
handlePeer m sock _ = do
    info@(_, PWHandshake{..}) <- receiveHandshake $ sourceSocket sock
    if pwHandshakeInfoHash == pmInfoHash m
        then do
            sendMessage sock PWHandshake { pwHandshakePeerId = pmPeerId m
                                         , pwHandshakeInfoHash = pmInfoHash m
                                         }
            peer <- startPeer (pmPeerEventsChan m) sock (Just info)
            atomically $ modifyTVar (pmMapVar m) (Map.insert (pId peer) peer)
        else void $ close sock

consumeChannel :: Chan a -> (a -> IO b) -> IO ()
consumeChannel c f = void $ forkIO $ forever $ do
    x <- readChan c
    _ <- forkIO $ void $ f x
    return ()
