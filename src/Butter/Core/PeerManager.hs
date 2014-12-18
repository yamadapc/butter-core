{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Butter.Core.PeerManager where

import Butter.Core.MetaInfo (InfoHash)
import Butter.Core.Peer (Peer(..), createConnection, receiveConnection)
import Butter.Core.PeerWire (PeerAddr, PeerId)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TBQueue, TChan, atomically, readTBQueue,
                               writeTChan)
import Control.Exception (SomeException, handle)
import Control.Monad (forever)
import Network (PortID(..), listenOn)
import Network.Socket (PortNumber(..), Socket(..), accept)

listenForPeers :: PortNumber
               -> PeerId
               -> InfoHash
               -> TChan Peer
               -> TChan SomeException
               -> IO (Socket, ThreadId)
listenForPeers port peerId infoHash outputChan errChan = do
    sock <- listenOn (PortNumber port)
    tid <- forkIO $ forever $ handle (atomically . writeTChan errChan) $ do
        (peersock, _) <- accept sock
        peer <- receiveConnection peerId infoHash peersock
        atomically $ writeTChan outputChan peer
    return (sock, tid)

waitConnectToPeers :: PeerId
                   -> InfoHash
                   -> TBQueue PeerAddr
                   -> TChan Peer
                   -> TChan SomeException
                   -> IO ThreadId
waitConnectToPeers peerId infoHash queue outputChan errChan =
    forkIO $ forever $ handle (atomically . writeTChan errChan) $ do
        addr <- atomically $ readTBQueue queue
        _ <- forkIO $ handle (atomically . writeTChan errChan) $ do
            peer <- createConnection peerId infoHash addr
            atomically $ writeTChan outputChan peer
        return ()
