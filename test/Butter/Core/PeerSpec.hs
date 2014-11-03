{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.PeerSpec where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.Socket
import Test.Hspec

import Butter.Core.Peer

specBinary :: Spec
specBinary = do
    -- Peer decoding examples stolen from `compact2string` npm module's
    -- tests
    it "decode :: ByteString -> Peer" $ do
        let p = decode (L.pack [0x0A, 0x0A, 0x0A, 0x05, 0xFF, 0x80]) :: Peer
            addr = SockAddrInet (PortNum (pPort p)) (pIp p)
        (show addr) `shouldBe` "10.10.10.5:65408"

    it "decode :: ByteString -> [Peer]" $ do
        let ps = decode (L.pack [0x0A, 0x0A, 0x0A, 0x05, 0x00, 0x80, 0x64, 0x38,
                                 0x3a, 0x63, 0x6f, 0x6d]) :: [Peer]
            addrs = map (\p -> SockAddrInet (PortNum (pPort p)) (pIp p)) ps
        (map show addrs) `shouldBe` [ "10.10.10.5:128", "100.56.58.99:28525" ]

    it "encode :: PMMessage (PMHandshake) -> ByteString" $ do
        let msg = PWHandshake (B.pack [0x4d, 0x75, 0x34, 0x74, 0x42, 0x9d, 0x81,
                                       0x7b, 0x80, 0xff, 0x9e, 0x0c, 0x44, 0x1c,
                                       0xa6, 0x60, 0xec, 0x5d, 0x24, 0x50])
                              "BUT-0123456789012345"
            b = encode msg
        (L.unpack b) `shouldBe` [0x13, 0x42, 0x69, 0x74, 0x54, 0x6f, 0x72, 0x72,
                                 0x65, 0x6e, 0x74, 0x20, 0x70, 0x72, 0x6f, 0x74,
                                 0x6f, 0x63, 0x6f, 0x6c,

                                 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                                 0x4d, 0x75, 0x34, 0x74, 0x42, 0x9d, 0x81, 0x7b,
                                 0x80, 0xff, 0x9e, 0x0c, 0x44, 0x1c, 0xa6, 0x60,
                                 0xec, 0x5d, 0x24, 0x50,

                                 66, 85, 84, 45, 48, 49, 50, 51, 52, 53, 54, 55,
                                 56, 57, 48, 49, 50, 51, 52, 53]

    it "encode :: PMMessage (PMKeepAlive)" $ do
        let msg = PWKeepAlive
            b = encode msg
        (L.unpack b) `shouldBe` [0x00]

    it "encode :: PMMessage (PWChoke)" $ do
        let msg = PWChoke
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x01, 0x00]

    it "encode :: PMMessage (PWUnchoke)" $ do
        let msg = PWUnchoke
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x01, 0x01]

    it "encode :: PMMessage (PWInterested)" $ do
        let msg = PWInterested
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x01, 0x02]

    it "encode :: PMMessage (PWNotInterested)" $ do
        let msg = PWNotInterested
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x01, 0x03]

    it "encode :: PMMessage (PWHave)" $ do
        let msg = PWHave 4
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x05, 0x04, 0x00, 0x00, 0x00,
                                 0x04]

    it "encode :: PMMessage (PWRequest)" $ do
        let msg = PWRequest 4 0 10
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x0d, 0x05, 0x00, 0x00, 0x00,
                                 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                 0x0a]

    it "encode :: PMMessage (PWPiece)" $ do
        let msg = PWPiece 4 0 "very 'treta' piece for testing"
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x27, 0x07, 0x00, 0x00, 0x00,
                                 0x04, 0x00, 0x00, 0x00, 0x00,

                                 118, 101, 114, 121, 32, 39, 116, 114, 101, 116,
                                 97, 39, 32, 112, 105, 101, 99, 101, 32, 102,
                                 111, 114, 32, 116, 101, 115, 116, 105, 110,
                                 103]

    it "encode :: PMMessage (PWCancel)" $ do
        let msg = PWCancel 10 3 20
            b = encode msg
        (L.unpack b) `shouldBe` [0x00, 0x00, 0x00, 0x0d, 0x08, 0x00, 0x00, 0x00,
                                 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00,
                                 0x14]

spec :: Spec
spec = describe "Binary instances" specBinary
