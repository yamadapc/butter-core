{-# LANGUAGE RecordWildCards #-}
module Butter.Core.Peer where

import Butter.Core.MetaInfo (InfoHash)
import Butter.Core.PeerWire (PeerAddr(..), PeerId, PeerWireMessage(..),
                             PWBlock, PWInteger, connectToPeer,
                             receiveHandshake, receiveMessage, sendMessage)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent.STM (TChan, TVar, atomically, newTVar, writeTChan,
                               writeTVar)
import qualified Data.ByteString as B (ByteString)
import Data.Conduit (ResumableSource)
import Data.Conduit.Network (sourceSocket)
import Network.Socket (Socket)

-- * Peers
-------------------------------------------------------------------------------

-- ** Peer data types

-- |
-- Represents a peer connection; doesn't currently encapsulate the peer
-- listening loop
data Peer = Peer { _pSource :: ResumableSource IO B.ByteString
                 , _pSocket :: Socket
                 , pId :: PeerId

                 , pIsChoked :: TVar Bool
                 , pAmChoked :: TVar Bool
                 , pIsInterested :: TVar Bool
                 , pAmInterested :: TVar Bool
                 }

instance Show Peer where
    show Peer{..} = concat [ "Peer { ", show pId, ", ... }" ]

-- |
-- Events broadcasted from a peer connection. This should be what is
-- required to maintain the global state in sync
data PeerMessage = ConnectionClosed
                 | BlockDownload { bdIndex :: PWInteger
                                 , bdBegin :: PWInteger
                                 , bdBlock :: PWBlock
                                 }
                 | BlockCancel { bcIndex  :: PWInteger
                               , bcBegin  :: PWInteger
                               , bcLength :: PWInteger
                               }
                 | BlockRequest { brIndex  :: PWInteger
                                , brBegin  :: PWInteger
                                , brLength :: PWInteger
                                }
                 | BlockHave { bhIndex :: PWInteger
                             }

-- ** Creating and listening to connections

-- |
-- Creates a new peer object out of a write channel, of 'PeerEvent's and
-- a 'PeerAddr'. It'll attempt to connect to the peer; throw if it fails to
-- do so and start listening for messages on this socket.
--
-- Whenever a new peer event comes in, it'll send it upstream through the
-- write channel. This assumes there's some manager entity watching over
-- the channel.
createConnection :: PeerId            -- ^ The local peer's id
                 -> InfoHash          -- ^ The `info_hash` for the download
                 -> PeerAddr          -- ^ A peer's address
                 -> IO Peer
createConnection localPeerId infoHash addr = do
    sock <- connectToPeer addr
    sendMessage sock $ PWHandshake infoHash localPeerId
    tp <- receiveHandshake (sourceSocket sock) >>= validateHSTup infoHash
    newHSPeer sock tp

-- |
-- Converts a 'Socket' representing an incomming connection from a peer,
-- into a stablished 'Peer' object, after handshaking it.
receiveConnection :: PeerId -> InfoHash -> Socket -> IO Peer
receiveConnection localPeerId infoHash sock = do
    tp <- receiveHandshake (sourceSocket sock) >>= validateHSTup infoHash
    peer <- newHSPeer sock tp
    sendHandshake infoHash localPeerId peer
    return peer

-- |
-- Takes a peer and a message channel, starts to listen for messages from
-- the peer and feeds the channel with events as they come in.
listenPeer :: Peer -> TChan PeerMessage -> IO ()
listenPeer peer@Peer{..} writechan = do
    (rsrc, message) <- receiveMessage _pSource
    putStrLn $ "Got message" ++ show message

    atomically $ case message of
        PWKeepAlive -> return ()
        PWChoke ->
            writeTVar pIsChoked True
        PWUnchoke ->
            writeTVar pIsChoked False
        PWInterested ->
            writeTVar pIsInterested True
        PWNotInterested ->
            writeTVar pIsInterested False
        PWRequest idx beg len ->
            writeTChan writechan (BlockRequest idx beg len)
        PWHave idx ->
            writeTChan writechan (BlockHave idx)
        PWPiece idx beg blk ->
            writeTChan writechan (BlockDownload idx beg blk)
        PWCancel idx beg len ->
            writeTChan writechan (BlockCancel idx beg len)
        _ -> return ()

    listenPeer peer { _pSource = rsrc } writechan


-- * Utility functions
-------------------------------------------------------------------------------

-- |
-- Creates a peer connection out of its minimum components, with sane
-- defaults.
newPeer :: Socket -> ResumableSource IO B.ByteString -> PeerId -> IO Peer
newPeer sock rsrc peerId = atomically $ Peer rsrc sock <$> pure peerId
                                                       <*> newTVar True
                                                       <*> newTVar True
                                                       <*> newTVar False
                                                       <*> newTVar False

-- |
-- Creates a peer connection out of a 'Socket' and a tuple of a resumable
-- source and  a 'PWHandshake' (fails if the 'PWMessage' received isn't
-- a handshake).
--
-- This is a helper to allow the composition:
-- > newHSPeer sock <=< validateHStup infoHash <=< receiveHandshake
-- Rather than:
-- > let src = sourceSocket sock
-- > (rsrc, PWHanishake peerInfoHash peerId) <- receiveHandshake src
-- > validateInfoHash infoHash peerInfoHash
-- > newPeer sock rsrc peerId
newHSPeer :: Socket -> (ResumableSource IO B.ByteString, PeerWireMessage)
          -> IO Peer
newHSPeer sock (rsrc, PWHandshake _ pid) = newPeer sock rsrc pid
newHSPeer _ (_, _) = fail ""

-- |
-- Validates a tuple containing 'PWHandshake' as its second member. For use
-- with the return value of 'receiveHandshake'. Returns the argument if
-- valid, for further composition.
validateHSTup :: InfoHash -> (a, PeerWireMessage) -> IO (a, PeerWireMessage)
validateHSTup ih tup@(_, PWHandshake pih _) =
    if pih == ih
        then return tup
        else fail ""
validateHSTup _  _ = fail ""

{-# ANN module "HLint: ignore Top-level binding with no type signature" #-}
sendPeerMessage :: PeerWireMessage -> Peer -> IO ()
sendPeerMessage msg peer = sendMessage (_pSocket peer) msg

sendKeepAlive, sendChoke, sendUnchoke, sendInterested, sendNotInterested
    :: Peer -> IO ()
sendHandshake :: InfoHash -> PeerId -> Peer -> IO ()
sendKeepAlive     = sendPeerMessage PWKeepAlive
sendChoke         = sendPeerMessage PWChoke
sendUnchoke       = sendPeerMessage PWUnchoke
sendInterested    = sendPeerMessage PWInterested
sendNotInterested = sendPeerMessage PWNotInterested
sendHandshake i p = sendPeerMessage $ PWHandshake i p
