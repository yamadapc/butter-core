{-# LANGUAGE OverloadedStrings #-}
module ButterCoreTorrentSpec where

import Data.BEncode (encode, decode)
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Lazy as L (toStrict)
import Butter.Core.Torrent (Torrent, fromBEncode, miAnnounce, toBEncode)
import Test.Hspec

spec :: Spec
spec = do
    describe "fromBEncode" $ do
        it "decodes the announce URLs" $ do
            f <- B.readFile "test.torrent"
            let Right to = decode f >>= fromBEncode :: Either String Torrent
            miAnnounce to `shouldBe` "http://torrent.ubuntu.com:6969/announce"

    describe "toBEncode" $ do
        it "encodes torrents back if read with `fromBEncode`" $ do
            f <- B.readFile "test.torrent"
            let Right to = decode f >>= fromBEncode :: Either String Torrent
                f2 = encode $ toBEncode to
            f `shouldBe` L.toStrict f2
