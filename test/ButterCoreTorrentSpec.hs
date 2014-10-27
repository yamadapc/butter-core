{-# LANGUAGE OverloadedStrings #-}
module ButterCoreTorrentSpec where

import Data.BEncode
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Base16 as Base16 (encode)
import Test.Hspec

import Butter.Core.Torrent

spec :: Spec
spec = do
    describe "fromBEncode" $ do
        it "decodes the announce URLs" $ do
            f <- B.readFile "test1.torrent"
            let Right to = decode f >>= fromBEncode :: Either String Torrent
            miAnnounce to `shouldBe` "http://torrent.ubuntu.com:6969/announce"
            let i = miInfo to
            fiName i `shouldBe` Just "ubuntu-14.04-desktop-amd64.iso"
            fiLength i `shouldBe` Just 1010827264
            fiPieceLength i `shouldBe` 524288

    describe "fiHash" $ do
        it "gets the hash for the `info` torrent node on test1.torrent" $ do
            f <- B.readFile "test1.torrent"
            let Right to = decode f >>= fromBEncode :: Either String Torrent
            Base16.encode (fiHash $ miInfo to)
              `shouldBe` "4d753474429d817b80ff9e0c441ca660ec5d2450"

        it "gets the hash for the `info` torrent node" $ do
            f <- B.readFile "test2.torrent"
            let Right to = decode f >>= fromBEncode :: Either String Torrent
            Base16.encode (fiHash $ miInfo to)
              `shouldBe` "1097bfd01b6ff5f1bd19b243e1535b3e7f486b9b"
