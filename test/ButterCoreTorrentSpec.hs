{-# LANGUAGE OverloadedStrings #-}
module ButterCoreTorrentSpec where

import Data.BEncode (encode, decode)
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Base16 as Base16 (encode)
import qualified Data.ByteString.Lazy as L (toStrict)
import Butter.Core.Torrent (Torrent, fromBEncode, miAnnounce, toBEncode, infoHash)
import Test.Hspec

spec :: Spec
spec = do
    describe "fromBEncode" $ do
        it "decodes the announce URLs" $ do
            f <- B.readFile "test.torrent"
            let Right to = decode f >>= fromBEncode :: Either String Torrent
            miAnnounce to `shouldBe` "http://torrent.ubuntu.com:6969/announce"

    describe "infoHash" $ do
        it "gets the hash for the `info` torrent node" $ do
            f <- B.readFile "test.torrent"
            let Right to = decode f >>= fromBEncode :: Either String Torrent
            Base16.encode (infoHash to)
              `shouldBe` "4d753474429d817b80ff9e0c441ca660ec5d2450"
