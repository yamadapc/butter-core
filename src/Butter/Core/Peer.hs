{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.Peer where

import Butter.Core.PeerWire (PeerAddr(..), PeerId, PeerWireMessage(..),
                             PWBlock, PWInteger, connectToPeer,
                             receiveHandshake, receiveMessage)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar,
                               readTVarIO, writeTVar)
import Control.Monad (forever)
import qualified Data.ByteString as B (ByteString)
import Data.Conduit (ResumableSource)
import Data.Conduit.Network (sourceSocket)
import Network.Socket (Socket)

data Peer = Peer { _pSocket :: Socket
                 , _pSource :: TVar (ResumableSource IO B.ByteString)
                 , pId :: PeerId

                 , pIsChoked :: TVar Bool
                 , pAmChoked :: TVar Bool
                 , pIsInterested :: TVar Bool
                 , pAmInterested :: TVar Bool

                 , pPieces :: TVar [PWInteger]
                 }

data PeerEvent = ConnectionClosed
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

-- |
-- Creates a new peer object out of a write channel, of 'PeerEvent's and
-- a 'PeerAddr'. It'll attempt to connect to the peer; throw if it fails to
-- do so and start listening for messages on this socket.
--
-- Whenever a new peer event comes in, it'll send it upstream through the
-- write channel. This assumes there's some manager entity watching over
-- the channel.
createPeer :: Chan PeerEvent -- ^ The channel events will be written to
           -> PeerAddr       -- ^ A peer address, obtained with PeerWire.decode
           -> IO Peer
createPeer writechan addr = connectToPeer addr >>= startPeer writechan

-- |
-- The same as 'createPeer' but receives an already connected 'Socket'.
-- This is to be used when accepting peer connections, rather than starting
-- them. It assumes that the client's handshake has already been sent.
startPeer :: Chan PeerEvent -> Socket -> IO Peer
startPeer writechan sock = do
    let src = sourceSocket sock
    (rsrc, PWHandshake{..}) <- receiveHandshake src
                                   :: IO (ResumableSource IO B.ByteString,
                                          PeerWireMessage)

    (rsrcV, ic, ac, ii, ai, ps) <- atomically $ (,,,,,)
                                       <$> newTVar rsrc
                                       <*> newTVar True  <*> newTVar True
                                       <*> newTVar False <*> newTVar False
                                       <*> newTVar []

    let peer = Peer { _pSocket = sock
                    , _pSource = rsrcV
                    , pId = pwHandshakePeerId
                    , pIsChoked = ic
                    , pAmChoked = ac
                    , pIsInterested = ii
                    , pAmInterested = ai
                    , pPieces = ps
                    }

    _ <- forkIO $ listenPeer writechan peer

    return peer

listenPeer :: Chan PeerEvent -> Peer -> IO ()
listenPeer writechan Peer{..} = forever $ do
    src <- readTVarIO _pSource
    (rsrc, message) <- receiveMessage src

    let updateSource = writeTVar _pSource rsrc
        updateSourceIO = atomically updateSource
        update stm = atomically $ updateSource >> stm

    case message of
        PWKeepAlive ->
            updateSourceIO
        PWChoke ->
            update $ writeTVar pIsChoked True
        PWUnchoke ->
            update $ writeTVar pIsChoked False
        PWInterested ->
            update $ writeTVar pIsInterested True
        PWNotInterested ->
            update $ writeTVar pIsInterested False
        PWRequest idx beg len -> do
            updateSourceIO
            writeChan writechan (BlockRequest idx beg len)
        PWHave idx ->
            update $ modifyTVar pPieces (idx:)
        PWPiece idx beg blk -> do
            updateSourceIO
            writeChan writechan (BlockDownload idx beg blk)
        PWCancel idx beg len -> do
            updateSourceIO
            writeChan writechan (BlockCancel idx beg len)
        _ -> updateSourceIO
