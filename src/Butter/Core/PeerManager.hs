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
import Control.Monad (forever)
import Network (PortID(..), listenOn)
import Network.Socket (PortNumber(..), accept)

listenForPeers :: PortNumber -> PeerId -> InfoHash -> TChan Peer -> IO ThreadId
listenForPeers port peerId infoHash outputChan = do
    sock <- listenOn (PortNumber port)
    forkIO $ forever $ do
        (peersock, _) <- accept sock
        peer <- receiveConnection peerId infoHash peersock
        atomically $ writeTChan outputChan peer

waitConnectToPeers :: PeerId -> InfoHash -> TBQueue PeerAddr -> TChan Peer -> IO ThreadId
waitConnectToPeers peerId infoHash queue outputChan =
    forkIO $ forever $ do
        addr <- atomically $ readTBQueue queue
        _ <- forkIO $ do
            peer <- createConnection peerId infoHash addr
            atomically $ writeTChan outputChan peer
        return ()
