{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.PeerWire (
                            -- ** PeerAddr data types
                              PeerAddr(..)
                            , PeerId
                            , newPeerId
                            -- ** Peer Wire Protocol
                            , PeerWireMessage(..)
                            , PWBlock
                            , PWInteger
                            , connectToPeer
                            , receiveHandshake
                            , receiveMessage
                            , sendMessage
                            -- ** Binary parsing functions
                            , getAll
                            , putAll
                            -- ** Exported from `Data.Binary`
                            , Binary(..)
                            , encode
                            , decode
                            )
  where

import Butter.Core.MetaInfo (InfoHash)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (AssertionFailed(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Binary (Binary, encode, decode, get, put)
import Data.Binary.Get (Get, isEmpty, getByteString, getWord16le, getWord32le,
                        skip)
import Data.Binary.Put (Put, putByteString, putWord16le, putWord32le)
import qualified Data.ByteString as B (ByteString, length)
import qualified Data.ByteString.Char8 as C (pack)
import Data.Conduit (ResumableSource, Source, ($$+), ($$++))
import Data.Conduit.Serialization.Binary (sinkGet)
import Data.Time (formatTime, getCurrentTime)
import Data.Word (Word8, Word32)
import GHC.Generics (Generic)
import Network.Socket (Family(..), PortNumber(..), SockAddr(..), Socket,
                       SocketType(..), connect, defaultProtocol, socket)
import Network.Socket.ByteString.Lazy (sendAll)
import System.Locale (defaultTimeLocale)
import System.Posix.Process (getProcessID) -- sigh...

-- |
-- An abstraction wrapper for peer socket addresses
newtype PeerAddr = PeerAddr SockAddr
  deriving(Generic, Eq, Ord)

instance Show PeerAddr where
    show (PeerAddr x) = show x

type PeerId = B.ByteString

-- |
-- Generates a new peer id randomly
newPeerId :: IO PeerId
newPeerId = do
    t <- reverse <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime
    pid <- getProcessID
    return $ C.pack $ take 20 $ "BU-" ++ show pid ++ t ++ repeat '0'

-- |
-- Connects to a peer and returns an open socket with it. Assumes we're
-- inside of 'withSocketsDo'.
connectToPeer :: PeerAddr -> IO Socket
connectToPeer (PeerAddr addr) = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr
    return sock

-- |
-- Consumes a PWHandshake from some 'ByteString' conduit 'Source' and
-- returns it along with the resumable unconsumed part of the source. Fails
-- if the message received isn't a handshake.
--
-- This function isn't strictly necessary given
-- @receiveMessage . newResumableSource@ and the appropriate assertion.
receiveHandshake :: MonadThrow m
                 => Source m B.ByteString
                 -> m (ResumableSource m B.ByteString, PeerWireMessage)
receiveHandshake s = do
    (rs, pw) <- s $$+ sinkGet (get :: Get PeerWireMessage)
    case pw of
        (PWHandshake _ _) -> return (rs, pw)
        _ -> throwM $ AssertionFailed ("Expected handshake, but got: " ++ show pw)

-- |
-- Consumes a PeerWireMessage from some 'ByteString' conduit
-- 'ResumableSource' and returns it along with the resumable unconsumed
-- part of the source.
receiveMessage :: MonadThrow m
               => ResumableSource m B.ByteString
               -> m (ResumableSource m B.ByteString, PeerWireMessage)
receiveMessage s = s $$++ sinkGet (get :: Get PeerWireMessage)

-- |
-- Sends a peerwire message through a socket
sendMessage :: Socket -> PeerWireMessage -> IO ()
sendMessage s m = sendAll s $ encode m

-- |
-- The Bittorrent protocol name, used as a constant in handshake pw messages
protocolName :: B.ByteString
protocolName = "BitTorrent protocol"

protocolNameLength :: Word8
protocolNameLength = fromIntegral $ B.length protocolName

type PWInteger = Word32
type PWBlock = B.ByteString

-- |
-- Represents a message in the PeerWire protocol
data PeerWireMessage = PWHandshake { pwHandshakeInfoHash :: !InfoHash
                                   , pwHandshakePeerId   :: !PeerId
                                   }
                     | PWKeepAlive
                     | PWChoke
                     | PWUnchoke
                     | PWInterested
                     | PWNotInterested
                     | PWHave { pwHaveIndex :: !PWInteger
                              }
                     | PWBitfield !B.ByteString
                     | PWRequest { pwRequestIndex  :: !PWInteger
                                 , pwRequestBegin  :: !PWInteger
                                 , pwRequestLength :: !PWInteger
                                 }
                     | PWPiece { pwPieceIndex :: !PWInteger
                               , pwPieceBegin :: !PWInteger
                               , pwPieceBlock :: !PWBlock
                               }
                     | PWCancel { pwCancelIndex  :: !PWInteger
                                , pwCancelBegin  :: !PWInteger
                                , pwCancelLength :: !PWInteger
                                }
                     -- PWPort
  deriving(Eq, Ord, Show)

instance Binary PeerWireMessage where
    get = do
        l <- getMessageLength
        if l /= 0
            then do
                t <- get :: Get Word8
                case t of
                    0 -> return PWChoke
                    1 -> return PWUnchoke
                    2 -> return PWInterested
                    3 -> return PWNotInterested

                    4 -> PWHave <$> get
                    5 -> PWBitfield <$> getByteString (fromIntegral l)
                    6 -> PWRequest <$> get <*> get <*> get
                    7 -> PWPiece <$> get
                                 <*> get
                                 <*> getByteString (fromIntegral (l - 1 - 8))
                    8 -> PWCancel <$> get
                                  <*> get
                                  <*> get
                    _ -> do
                        _ <- skip 15
                        _ <- skip 8 -- `reserved` protocol field
                        PWHandshake <$> getByteString 20
                                    <*> getByteString 20
            else return PWKeepAlive

    put PWHandshake{..} = do
        put protocolNameLength
        putByteString protocolName
        putByteString "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
        putByteString pwHandshakeInfoHash
        putByteString pwHandshakePeerId

    put PWKeepAlive     = put (0 :: PWInteger)
    put PWChoke         = pwEmpty 0
    put PWUnchoke       = pwEmpty 1
    put PWInterested    = pwEmpty 2
    put PWNotInterested = pwEmpty 3

    put PWHave{..} = do
        put (5 :: PWInteger)
        putPWId 4
        put pwHaveIndex

    put PWRequest{..} = do
        put (13 :: PWInteger)
        putPWId 6
        put pwRequestIndex
        put pwRequestBegin
        put pwRequestLength

    put PWPiece{..} = do
        put (9 + fromIntegral (B.length pwPieceBlock) :: PWInteger)
        putPWId 7
        put pwPieceIndex
        put pwPieceBegin
        putByteString pwPieceBlock

    put PWCancel{..} = do
        put (13 :: PWInteger)
        putPWId 8
        put pwCancelIndex
        put pwCancelBegin
        put pwCancelLength

    put _ = undefined

-- |
-- Gets a peerwire message's length
getMessageLength :: Get PWInteger
getMessageLength = get

-- |
-- "Puts" an empty PeerWire message of ID @n@
pwEmpty :: Word8 -> Put
pwEmpty n = put (1 :: PWInteger) >> putPWId n

-- |
-- Puts the message id
putPWId :: Word8 -> Put
putPWId = put

instance Binary PeerAddr where
    get = do
        ip <- getWord32le
        pn <- getWord16le
        return $ PeerAddr $ SockAddrInet (PortNum pn) ip

    put (PeerAddr (SockAddrInet (PortNum pPort) pIp)) =
        putWord32le pIp >> putWord16le pPort
    put _ = fail "IPv6 is not currently supported."

instance Binary [PeerAddr] where
    get = getAll
    put = putAll

-- |
-- Puts a list of 'Binary' instance elements directly concatenating their
-- serialized values
putAll :: Binary a => [a] -> Put
putAll = mapM_ put

-- |
-- Consumes all elements, getting a single type. Modeled after @binary@'s
-- internal funtion @getMany@
getAll :: Binary a => Get [a]
getAll = go []
  where go acc = get >>= \x -> isEmpty >>= \case
            True  -> return $ reverse (x:acc)
            False -> seq x $ (x:) <$> getAll
