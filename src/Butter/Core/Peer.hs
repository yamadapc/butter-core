{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.Peer (
                        -- ** Peer data types
                          Peer(..)
                        , PeerId
                        , newPeerId
                        -- ** Peer Wire Protocol
                        , PeerWireMessage(..)
                        , PWBlock
                        , PWInteger
                        , connectToPeer
                        , sendPeerHandshake
                        -- ** Binary parsing functions
                        , getAll
                        , putAll
                        -- ** Exported from `Data.Binary`
                        , Binary(..)
                        , encode
                        , decode
                        )
  where

import Butter.Core.Util
import Control.Applicative ((<$>))
import Data.Binary (Binary, encode, decode, get, put)
import Data.Binary.Get (Get, isEmpty, getByteString)
import Data.Binary.Put (Put)
import qualified Data.ByteString as B (ByteString, length)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.ByteString.Char8 as C (pack)
import Data.Time (formatTime, getCurrentTime)
import Data.Word (Word8, Word16, Word32)
import GHC.Generics (Generic)
import Network.Socket
import System.Locale (defaultTimeLocale)
import System.Posix.Process (getProcessID) -- sigh...

data Peer = Peer { pIp   :: HostAddress -- type HostAddress = Word32
                 , pPort :: Word16      -- newtype PortNumber = PortNum Word16
                 }
  deriving(Generic, Eq, Ord, Show)

type PeerId = B.ByteString

-- |
-- Generates a new peer id randomly
newPeerId :: IO PeerId
newPeerId = do
    t <- reverse <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime
    pid <- getProcessID
    return $ C.pack $ take 20 $ "BU-" ++ show pid ++ t ++ repeat '0'

-- |
-- The Bittorrent protocol name, used as a constant in handshake pw messages
protocolName :: B.ByteString
protocolName = "BitTorrent protocol"

protocolNameLength :: PWInteger
protocolNameLength = fromIntegral $ B.length protocolName

type PWInteger = Word32
type PWBlock = L.ByteString

-- |
-- Represents a message in the PeerWire protocol
data PeerWireMessage = PWHandshake { pwHandshakeInfoHash :: B.ByteString
                                   , pwHandshakePeerId   :: B.ByteString
                                   }
                     | PWKeepAlive
                     | PWChoke
                     | PWUnchoke
                     | PWInterested
                     | PWNotInterested
                     | PWHave { pwHaveIndex :: PWInteger
                              }
                     | PWBitfield [Word8]
                     | PWRequest { pwRequestIndex  :: PWInteger
                                 , pwRequestBegin  :: PWInteger
                                 , pwRequestLength :: PWInteger
                                 }
                     | PWPiece { pwPieceIndex  :: PWInteger
                               , pwPieceCancel :: PWInteger
                               , pwPieceBlock  :: PWBlock
                               }
                     | PWCancel
                     -- Temporary cheat:
                     | PWBinary B.ByteString
                     -- PWPort

-- |
-- Connects to a peer and returns an open socket with it. Assumes we're
-- inside of 'withSocketsDo'.
connectToPeer :: Peer -> IO Socket
connectToPeer Peer{..} = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock sockaddr
    return sock
  where sockaddr = SockAddrInet (PortNum pPort) pIp

-- |
-- Sends the handshake peerwire message through a socket
sendPeerHandshake :: Socket -> IO ()
sendPeerHandshake = undefined

instance Binary PeerWireMessage where
    get = do
        l <- get :: Get PWInteger
        bs <- getByteString $ fromIntegral l
        return $ PWBinary bs
        --if l /= 0
            --then do
                --t <- get :: Get Word8
                --case t of
                    --0 -> return PWChoke
                    --1 -> return PWUnchoke
                    --2 -> return PWInterested
                    --3 -> return PWNotInterested
                    --4 -> do
                        --i <- get
                        --return $ PWHave i
                    --5 -> do
                        --r <- getByteString l
                        --PWBitfield r
                    --6 ->
                        --PWRequest
                    --7 ->
                        --PWPiece
                    --8 ->
                        --PWCancel
                    --_ -> fail "Invalid message id."
            --else return PWKeepAlive

    put PWHandshake{..} = do
        put protocolNameLength
        put protocolName
        putAll ([0, 0, 0, 0, 0, 0, 0, 0] :: [PWInteger])
        put pwHandshakeInfoHash
        put pwHandshakePeerId

    put PWKeepAlive     = put (0 :: Word8)
    put PWChoke         = pwEmpty 0
    put PWUnchoke       = pwEmpty 1
    put PWInterested    = pwEmpty 2
    put PWNotInterested = pwEmpty 3
    put _               = undefined

-- |
-- "Puts" an empty PeerWire message of ID @n@
pwEmpty :: Word8 -> Put
pwEmpty n = put (0 :: PWInteger) >> put n

instance Binary Peer
instance Binary [Peer] where
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
