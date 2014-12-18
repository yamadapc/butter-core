module Butter.Core.BlockDownload
  where

import Butter.Core.Block
import Butter.Core.Peer
import Butter.Core.MetaInfo (Piece(..))
import Butter.Core.Torrent
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad -- (filterM, unless, void)
import qualified Data.ByteString.Lazy -- ()

downloadBlock :: BlockId -> [Peer] -> IO (Async BlockBody)
downloadBlock blockId owningPeers = do
    candidates <- filterM isDownloadCandidate owningPeers
    mapConcurrently (expressInterestIn blockId) candidates
    return undefined

expressInterestIn :: BlockId -> Peer -> IO ()
expressInterestIn blockId peer = do
    let amInterestedTVar = pAmInterested peer
        requestedTVar = pRequestedBlocks peer

    amInterested <- readTVarIO amInterestedTVar
    unless amInterested $ do
        sendInterested peer
        atomically $ writeTVar amInterestedTVar True

    requested <- elem blockId <$> readTVarIO requestedTVar
    unless requested $ do
        sendRequest blockId peer
        atomically $ modifyTVar requestedTVar (\bs -> blockId:bs)

isDownloadCandidate :: Peer -> IO Bool
isDownloadCandidate p = readTVarIO (pIsChoked p)

downloadPiece :: Piece -> TorrentDownload-> IO Data.ByteString.Lazy.ByteString
downloadPiece = undefined

defaultBlockSize :: BlockSize
defaultBlockSize = 16000
