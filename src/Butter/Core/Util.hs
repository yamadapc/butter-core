-- |
-- General shared logic.
--
-- NOTE
-- This module is to be replaced with a complete better implementation of
-- an URL encoding library, which is currently being worked on.
module Butter.Core.Util where

import Data.Char (ord)
import Data.Binary as Binary (Binary, decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
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

encodeS :: Binary a => a -> B.ByteString
encodeS = L.toStrict . Binary.encode

decodeS :: Binary a => B.ByteString -> a
decodeS = Binary.decode . L.fromStrict
