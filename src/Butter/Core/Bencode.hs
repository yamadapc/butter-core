{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Butter.Core.Bencode where

import Control.Applicative
import Data.Attoparsec.Char8 as AP
import qualified Data.ByteString as B
import Data.Map as M (Map, fromList)

data BEValue = BEInt Integer
             | BEString B.ByteString
             | BEList [BEValue]
             | BEDict (Map B.ByteString BEValue)
  deriving(Eq, Ord, Show)

parseBEValues :: Parser [BEValue]
parseBEValues = many parseBEValue

parseBEValue :: Parser BEValue
parseBEValue = parseBEInt    <|>
               parseBEString <|>
               parseBEList   <|>
               parseBEDict

parseBEInt :: Parser BEValue
parseBEInt = do
    _ <- char 'i'
    i <- decimal
    _ <- char 'e'
    return $ BEInt i

parseBEString :: Parser BEValue
parseBEString = do
    l <- decimal
    _ <- char ':'
    BEString <$> AP.take l

parseBEList :: Parser BEValue
parseBEList = do
    _ <- char 'l'
    bs <- many parseBEValue
    _ <- char 'e'
    return $ BEList bs

parseBEDict :: Parser BEValue
parseBEDict = do
    _ <- char 'd'

    kvs <- many $ do
      BEString k <- parseBEString
      v <- parseBEValue
      return (k, v)
    _ <- char 'e'

    return $ BEDict $ M.fromList kvs
