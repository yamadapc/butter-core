{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Butter.Core.Peer ( Peer(..)
                        , PeerId
                        , newPeerId
                        , getAll
                        , putAll
                        -- Exported from `Data.Binary`
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
import qualified Data.ByteString.Char8 as C (pack)
import Data.Time (formatTime, getCurrentTime)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import System.Locale (defaultTimeLocale)
import System.Posix.Process (getProcessID) -- sigh...

type PeerId = B.ByteString

data Peer = Peer { pIp   :: Word32
                 , pPort :: Word16
                 }
  deriving(Generic, Eq, Ord, Show)

instance Binary Peer
instance Binary [Peer] where
    get = getAll
    put = putAll

-- |
-- Generates a new peer id randomly
newPeerId :: IO PeerId
newPeerId = do
    t <- reverse <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime
    pid <- getProcessID
    return $ C.pack $ take 20 $ "BU-" ++ show pid ++ t

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
