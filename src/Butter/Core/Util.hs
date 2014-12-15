{-# LANGUAGE LambdaCase #-}
-- |
-- General shared logic.
--
-- NOTE
-- Part of this module is to be replaced with a complete better
-- implementation of an URL encoding library, which is currently being
-- worked on.
module Butter.Core.Util where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, TBQueue, atomically, isFullTBQueue,
                               writeTBQueue)
import Control.Monad (unless)
import Data.Binary (Binary(..), Get, Put, get, put)
import Data.Binary.Get (isEmpty)
import Data.Char (ord)
import Data.List (partition)
import Numeric (showHex)

-- |
-- URL Encodes a String
-- (was mostly copied from the combinatorrent copy of the old HTTP library,
-- I'm working on a separate library which will expose more general and
-- efficient version of this)
urlEncode' :: String -> String
urlEncode' [] = []
urlEncode' (h:t) = let str = if reserved (ord h) then escape h else [h]
                  in str ++ urlEncode' t
 where reserved x | x >= ord 'a' && x <= ord 'z' = False
                  | x >= ord 'A' && x <= ord 'Z' = False
                  | x >= ord '0' && x <= ord '9' = False
                  | x <= 0x20 || x >= 0x7F = True
                  | otherwise = x `elem` map ord ";/?:@&=+,${}|\\^[]`<>#%\""
       escape x = '%': showHex (ord x) "" -- This is wrong usage of `showHex`

-- |
-- Encodes an array of key-value tuples into an URL-encoded querystring
-- (look at the comment in `urlEncode`)
urlEncodeVars :: [(String, String)] -> String
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode' n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode' y) (urlEncode' v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff
urlEncodeVars [] = []

-- |
-- IO Version of 'writeList2TBQueue'
writeList2TBQueueIO :: TBQueue a -> [a] -> IO ()
writeList2TBQueueIO q xs = atomically $ writeList2TBQueue q xs

-- |
-- Tries to write all elements in a list to a TBQueue and fails silently if
-- its full.
-- Never blocks.
writeList2TBQueue :: TBQueue a -> [a] -> STM ()
writeList2TBQueue _ [] = return ()
writeList2TBQueue q (x:xs) = do
    f <- isFullTBQueue q
    unless f $ writeTBQueue q x >>
               writeList2TBQueue q xs

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

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = let (ys, xs') = splitAt n xs in ys : splitEvery n xs'
