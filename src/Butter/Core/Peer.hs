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

import Control.Applicative ((<$>))
import Data.Binary (Binary, encode, decode, get, put)
import Data.Binary.Get (Get, isEmpty)
import Data.Binary.Put (Put)
import qualified Data.ByteString as B (ByteString)
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

type PWInteger = Word32
type PWBlock = L.ByteString
data PeerWireMessage = PWKeepAlive
                     | PWChoke
                     | PWUnchoke
                     | PWNotInterested
                     | PWInterested
                     | PWHave { pwHaveIndex :: PWInteger
                              }
                     | PWBitField [Word8]
                     | PWRequest { pwRequestIndex  :: PWInteger
                                 , pwRequestBegin  :: PWInteger
                                 , pwRequestLength :: PWInteger
                                 }
                     | PWPiece { pwPieceIndex  :: PWInteger
                               , pwPieceCancel :: PWInteger
                               , pwPieceBlock  :: PWBlock
                               }
                     -- | PWPort

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

instance Binary Peer
instance Binary [Peer] where
    get = getAll
    put = putAll

-- |
-- Puts a list of `Binary` instance elements directly concatenating their
-- binary values.
putAll :: Binary a => [a] -> Put
putAll = mapM_ put

-- |
-- Consumes all elements, getting a single type. Modeled after `binary`'s
-- internal funtion `getMany`.
getAll :: Binary a => Get [a]
getAll = go []
  where go acc = get >>= \x -> isEmpty >>= \case
            True  -> return $ reverse (x:acc)
            False -> seq x $ (x:) <$> getAll
