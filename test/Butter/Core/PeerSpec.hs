{-# LANGUAGE RecordWildCards #-}
module Butter.Core.PeerSpec where

import Data.Binary
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

spec :: Spec
spec = describe "Binary instances" specBinary
