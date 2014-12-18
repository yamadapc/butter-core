{-# LANGUAGE OverloadedStrings #-}
module Butter.Core.BlockDownloadSpec
  where

import Butter.Core.Block
import Butter.Core.BlockDownload
import Butter.Core.Peer
import Butter.Core.PeerManager
import Butter.Core.PeerWire (PeerAddr(..), PeerId, newPeerId)
import Control.Applicative ((<$>))
import Control.Concurrent.Async (wait)
import Control.Concurrent.STM (newTChanIO)
import qualified Data.ByteString as ByteString
import Network.Socket (getSocketName)
import Test.Hspec

spec :: Spec
spec =
    describe "downloadBlock :: BlockId -> [Peer] -> IO (Async Block)" $ -- do
        it "eventually resolves to a downloaded block" $ do
            let blockId = BlockId undefined undefined undefined
            (ourPeerId, fakePeerAddr, expectedBlock) <- setupFakePeer blockId

            peer <- createConnection
                        ourPeerId
                        "01234567890123456789"
                        fakePeerAddr

            blockFuture <- downloadBlock blockId [peer]
            block <- wait blockFuture
            block `shouldBe` expectedBlock

        -- it "regardless of its position/size" $ do
        --     ourPeerId <- newPeerId
        --     fakePeer <- newFakePeer
        --     peer <- createConnection ourPeerId "01234567890123456789" fakePeer
        --     blockFuture <- downloadBlock (fst fakeBlockA) [peer]
        --     block <- wait blockFuture
        --     block `shouldBe` undefined

setupFakePeer :: BlockId -> IO (PeerId, PeerAddr, BlockBody)
setupFakePeer blockId = do
    ourPeerId <- newPeerId
    fakePeer <- newFakePeer
    return (ourPeerId, fakePeer, blockChunkFromId blockId fullFakeBlock)

blockChunkFromId :: BlockId -> ByteString.ByteString -> ByteString.ByteString
blockChunkFromId (BlockId _ i l) = ByteString.take l' . ByteString.drop i'
  where l' = fromIntegral l
        i' = fromIntegral i

fullFakeBlock :: ByteString.ByteString
fullFakeBlock = "random-data is here bla bla bla ignore me"

newFakePeer :: IO PeerAddr
newFakePeer = do
    ochan <- newTChanIO
    echan <- newTChanIO
    (sock, _) <- listenForPeers
                     3000 "butter-id" "01234567890123456789" ochan echan

    PeerAddr <$> getSocketName sock
