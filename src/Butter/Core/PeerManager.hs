{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Butter.Core.PeerManager where

import Butter.Core.MetaInfo (MetaInfo(..), FileInfo(fiHash))
import Butter.Core.Peer (Peer(..), PeerMessage(..), createConnection,
                         receiveConnection)
import Butter.Core.PeerWire (PeerId, PWInteger)
import Butter.Core.Tracker.Client (ClientOptions(..), TrackerClient(..),
                                   readPeerAddr)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (MVar, forkIO, newMVar, tryPutMVar, tryTakeMVar,
                           readMVar)
import Control.Concurrent.STM -- (TChan, TVar, atomically, modifyTVar, newTChanIO,
                               -- newTVarIO, readTVar, writeTChan)
import Control.Monad (void, when)
import qualified Data.Map.Strict as Map (Map, empty, insert, size)
import Network (PortID(..), listenOn)
import Network.Socket (PortNumber(..), accept)

data PeerManager =
    PeerManager { pmPeerId   :: PeerId
                , pmPort     :: PortNumber
                , pmMetaInfo :: MetaInfo

                , pmSource :: PeerSource

                , pmConnections :: TVar (Map.Map PeerId Peer)
                , pmPieces      :: TVar (Map.Map PWInteger [PeerId])
                , pmMsgsChan    :: TChan PeerMessage
                }

newPeerManager :: PeerId -> PortNumber -> MetaInfo -> PeerSource
               -> IO PeerManager
newPeerManager peerId port mi s =
    PeerManager peerId port mi s <$> newTVarIO Map.empty
                                 <*> newTVarIO Map.empty
                                 <*> newTChanIO

addPeer :: PeerManager -> Peer -> IO ()
addPeer m peer = do
    sz <- atomically $
        modifyTVar (pmConnections m) (Map.insert (pId peer) peer) >>
        Map.size <$> readTVar (pmConnections m)

    when (sz > 50) $ stopPeerSource (pmSource m)

-- ** Acceptor and Connector types

runAcceptor :: PeerId -> PortNumber -> MetaInfo -> IO PeerSource
runAcceptor peerId port mi = do
    m <- newMVar ()
    c <- newTChanIO
    sock <- listenOn $ PortNumber port
    let self = (m, c)

    void $ forkIO $ loop self sock

    return self
  where infoHash = fiHash $ miInfo mi
        loop self@(_, c) sock = guardRunning self $ do
            (peersock, _) <- accept sock
            void $ forkIO $ receiveConnection peerId infoHash peersock >>=
                            atomically . writeTChan c
            loop self sock

runConnector :: TrackerClient -> IO PeerSource
runConnector tc = do
    m <- newMVar ()
    c <- newTChanIO
    let self = (m, c)

    void $ forkIO $ loop self

    return self
  where opts = tcOptions tc
        peerId = cPeerId opts
        infoHash = cInfoHash opts
        loop self@(_, c) = guardRunning self $ do
            peerAddr <- readPeerAddr tc
            void $ forkIO $ createConnection peerId infoHash peerAddr >>=
                            atomically . writeTChan c
            loop self

-- *** Stopping mechanism

type PeerSource = (MVar (), TChan Peer)

mergePeerSources :: PeerSource -> PeerSource -> IO PeerSource
mergePeerSources (_, c1) (_, c2) = do
    m <- newMVar ()
    c <- mergeTChans c1 c2
    return (m, c)

resumePeerSource :: PeerSource -> IO ()
resumePeerSource (lock, _) = void $ tryPutMVar lock ()

stopPeerSource :: PeerSource -> IO ()
stopPeerSource (lock, _) = void $ tryTakeMVar lock

guardRunning :: PeerSource -> IO a -> IO a
guardRunning (lock, _) f = readMVar lock >> f

dupPeerSource :: PeerSource -> IO PeerSource
dupPeerSource (_, c) = (,) <$> newMVar () <*> atomically (dupTChan c)

mergeTChans :: TChan a -> TChan a -> IO (TChan a)
mergeTChans c1 c2 = do
  c <- newTChanIO
  void $ forkIO $ atomically $ readTChan c1 `orElse` readTChan c2 >>=
                               writeTChan c
  return c
