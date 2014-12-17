{-# LANGUAGE OverloadedStrings #-}
module Butter.Core.MetaInfoSpec where

import Data.BEncode
import qualified Data.Binary as Binary (decode, encode)
import qualified Data.ByteString as B (length, readFile)
import qualified Data.ByteString.Lazy as L (fromStrict, length, toStrict)
import qualified Data.ByteString.Base16 as Base16 (encode)
import Test.Hspec

import Butter.Core.MetaInfo

specBEncode :: Spec
specBEncode = do
    it "fromBEncode :: BValue -> Result MetaInfo" $ do
        f <- B.readFile "test/test1.torrent"
        let Right mi = decode f >>= fromBEncode :: Either String MetaInfo
        miAnnounce mi `shouldBe` "http://torrent.ubuntu.com:6969/announce"
        let i = miInfo mi
        fiName i `shouldBe` Just "ubuntu-14.04-desktop-amd64.iso"
        fiLength i `shouldBe` Just 1010827264
        fiPieceLength i `shouldBe` 524288

    it "toBEncode :: MetaInfo -> BValue" $ do
        f1 <- B.readFile "test/test1.torrent"
        let Right mi1 = decode f1 >>= fromBEncode :: Either String MetaInfo
            pieces = fiPieces $ miInfo mi1
            encoded = Binary.encode pieces

        mapM_ ((`shouldBe` 20) . B.length . pHash) pieces
        L.length encoded `shouldBe` fromIntegral (20 * length pieces)
        encode mi1 `shouldBe` L.fromStrict f1

    it "toBEncode :: FileInfo -> BValue" $ do
        f1 <- B.readFile "test/test1-infohash"
        let Right fi1 = decode f1 >>= fromBEncode :: Either String FileInfo
            f2 = encode fi1
            Right fi2 = decode (L.toStrict f2) >>= fromBEncode :: Either String FileInfo
        fi1 `shouldBe` fi2

    it "decode :: ByteString -> [Piece]" $ do
        f <- B.readFile "test/test1-pieces"
        let pieces = Binary.decode $
                L.fromStrict f :: [Piece]
        Binary.encode pieces `shouldBe` L.fromStrict f

    it "toBEncode :: FileNode -> BValue" $ do
        f1 <- B.readFile "test/test1-filenode"
        let Right fn1 = decode f1 :: Either String FileNode
            f2 = encode fn1
            Right fn2 = decode (L.toStrict f2) >>= fromBEncode :: Either String FileNode
        fn1 `shouldBe` fn2

    it "readMetaInfoFile :: FilePath -> IO MetaInfo" $ do
        mi <- readMetaInfoFile "test/test1.torrent"
        f <- B.readFile "test/test1.torrent"
        let Right mi' = decode f >>= fromBEncode :: Either String MetaInfo
        mi `shouldBe` mi'

    describe "fiHash" $ do
        it "gets the hash for the `info` torrent node on test1.torrent" $ do
            f <- B.readFile "test/test1.torrent"
            let Right mi = decode f >>= fromBEncode :: Either String MetaInfo
            Base16.encode (fiHash $ miInfo mi)
              `shouldBe` "4d753474429d817b80ff9e0c441ca660ec5d2450"

        it "gets the hash for the `info` torrent node on test2.torrent" $ do
            f <- B.readFile "test/test2.torrent"
            let Right mi = decode f >>= fromBEncode :: Either String MetaInfo
            Base16.encode (fiHash $ miInfo mi)
              `shouldBe` "1097bfd01b6ff5f1bd19b243e1535b3e7f486b9b"



spec :: Spec
spec = describe "BEncode instances" specBEncode
