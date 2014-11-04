{-# LANGUAGE OverloadedStrings #-}
module Butter.Core.PeerSpec where

import Control.Arrow (first)
import Control.Exception (AssertionFailed(..))
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit as C
import Network.Socket
import Test.Hspec

import Butter.Core.Peer

peerWireExamples :: [(L.ByteString, PeerWireMessage)]
peerWireExamples =
    map (first L.pack) [ ([0x13, 0x42, 0x69, 0x74, 0x54, 0x6f, 0x72, 0x72,
                           0x65, 0x6e, 0x74, 0x20, 0x70, 0x72, 0x6f, 0x74,
                           0x6f, 0x63, 0x6f, 0x6c,

                           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           0x4d, 0x75, 0x34, 0x74, 0x42, 0x9d, 0x81, 0x7b,
                           0x80, 0xff, 0x9e, 0x0c, 0x44, 0x1c, 0xa6, 0x60,
                           0xec, 0x5d, 0x24, 0x50,

                           66, 85, 84, 45, 48, 49, 50, 51, 52, 53, 54, 55,
                           56, 57, 48, 49, 50, 51, 52, 53],
                          PWHandshake (B.pack [0x4d, 0x75, 0x34, 0x74, 0x42,
                                               0x9d, 0x81, 0x7b, 0x80, 0xff,
                                               0x9e, 0x0c, 0x44, 0x1c, 0xa6,
                                               0x60, 0xec, 0x5d, 0x24, 0x50])
                                       "BUT-0123456789012345")
                       , ([0x00, 0x00, 0x00, 0x00], PWKeepAlive)
                       , ([0x00, 0x00, 0x00, 0x01, 0x00], PWChoke)
                       , ([0x00, 0x00, 0x00, 0x01, 0x01], PWUnchoke)
                       , ([0x00, 0x00, 0x00, 0x01, 0x02], PWInterested)
                       , ([0x00, 0x00, 0x00, 0x01, 0x03], PWNotInterested)
                       , ([0x00, 0x00, 0x00, 0x05, 0x04, 0x00, 0x00, 0x00, 0x04],
                          PWHave 4)
                       , ([0x00, 0x00, 0x00, 0x0d, 0x06, 0x00, 0x00, 0x00, 0x04,
                           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a],
                          PWRequest 4 0 10)
                       , ([0x00, 0x00, 0x00, 0x27, 0x07, 0x00, 0x00, 0x00,
                           0x04, 0x00, 0x00, 0x00, 0x00,

                           118, 101, 114, 121, 32, 39, 116, 114, 101, 116,
                           97, 39, 32, 112, 105, 101, 99, 101, 32, 102, 111,
                           114, 32, 116, 101, 115, 116, 105, 110, 103],
                          PWPiece 4 0 "very 'treta' piece for testing")
                       , ([0x00, 0x00, 0x00, 0x0d, 0x08, 0x00, 0x00, 0x00,
                           0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00,
                           0x14],
                          PWCancel 10 3 20)
                       ]

specBinary :: Spec
specBinary = do
    -- Peer decoding examples stolen from `compact2string` npm module's
    -- tests
    it "decode :: ByteString -> Peer" $ do
        let p = decode (L.pack [0x0A, 0x0A, 0x0A, 0x05, 0xFF, 0x80]) :: Peer
            addr = SockAddrInet (PortNum (pPort p)) (pIp p)
        show addr `shouldBe` "10.10.10.5:65408"

    it "decode :: ByteString -> [Peer]" $ do
        let ps = decode (L.pack [0x0A, 0x0A, 0x0A, 0x05, 0x00, 0x80, 0x64, 0x38,
                                 0x3a, 0x63, 0x6f, 0x6d]) :: [Peer]
            addrs = map (\p -> SockAddrInet (PortNum (pPort p)) (pIp p)) ps
        map show addrs `shouldBe` [ "10.10.10.5:128", "100.56.58.99:28525" ]

    it "decode :: ByteString -> PWMessage" $ do
        let testCase (input, expectedOutput) =
                decode input `shouldBe` expectedOutput

        mapM_ testCase peerWireExamples

    it "encode :: PWMessage -> ByteString" $ do
        let testCase (expectedOutput, input) = 
                encode input `shouldBe` expectedOutput

        mapM_ testCase peerWireExamples

spec :: Spec
spec = do
    describe "Binary instances" specBinary

    describe "receiveHandshake" $ do
        it "yields a peerwire handshake if present" $ do
            let tc = head peerWireExamples
                source = C.yield $ L.toStrict $ L.concat $
                                                map fst peerWireExamples

            (_, pw) <- receiveHandshake source
            pw `shouldBe` snd tc

        it "fails if any other message is found" $ do
            let source = C.yield $ L.toStrict $ L.concat $
                                                map fst (tail peerWireExamples)

            receiveHandshake source `shouldThrow` assertionFailure

    describe "receiveMessage" $ do
        it "works if the `Source` conduit is completelly consumed" $ do
            let handshakeTc = head peerWireExamples
                source = C.newResumableSource $ C.yield $ L.toStrict $
                                                          fst handshakeTc

            (_, pw) <- receiveMessage source
            pw `shouldBe` snd handshakeTc

        it "yields back a resumable source when messages are concatenated" $ do
            let msgs = take 2 peerWireExamples
                source = C.newResumableSource (C.yield $ L.toStrict $ L.concat $
                                               map fst msgs)

            (rs, pw) <- receiveMessage source
            pw `shouldBe` snd (head msgs)
            (_, pw') <- receiveMessage rs
            pw' `shouldBe` snd (last msgs)

  where assertionFailure :: AssertionFailed -> Bool
        assertionFailure _ = True
