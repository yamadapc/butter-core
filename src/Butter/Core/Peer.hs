{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
module Butter.Core.Peer ( Peer(..)
                        , Binary(..)
                        , decode
                        )
  where

import Control.Monad (liftM)
import Data.Binary (Binary, decode, get, put)
import Data.Binary.Get (Get, isEmpty)
import Data.Binary.Put (Put)
import Data.Word (Word16, Word32)
import GHC.Generics

data Peer = Peer { pIp   :: Word32
                 , pPort :: Word16
                 }
  deriving(Generic, Eq, Ord, Show)

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
            False -> seq x $ liftM (x:) getAll

-- TODO:
-- [ ] - Client State model
-- [ ] - Protocol Messages' serialization and deserialization
