{-# LANGUAGE DeriveDataTypeable #-}
module Butter.Core.Peer where

import qualified Data.ByteString as B (ByteString, unpack)
import Data.List.Split (chunksOf)
import Data.Typeable (Typeable)
import Data.Word (Word8)

data Peer = Peer { pIp   :: [Word8]
                 , pPort :: [Word8]
                 }
  deriving(Eq, Ord, Show, Typeable)

parsePeersString :: B.ByteString -> [Peer]
parsePeersString = map peerFromSubstring . chunksOf 6 . B.unpack
  where peerFromSubstring :: [Word8] -> Peer
        peerFromSubstring str = Peer { pIp   = take 4 str
                                     , pPort = drop 4 str
                                     }
