{-# LANGUAGE LambdaCase #-}
module Butter.Core where

import Butter.Core.BlockDownload
import Butter.Core.MetaInfo
import Butter.Core.Peer
import Butter.Core.Torrent
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Vector as Vector

type DownloadStrategy = TorrentDownload -> IO [DownloadCommand]
data DownloadCommand = DownloadPiece Piece Int Int
                     | Abort

startDownload :: TorrentDownload -> DownloadStrategy -> IO ()
startDownload td st = loop
  where loop = do
            stage <- tsStage <$> readTVarIO (tdStatus td)
            case stage of
                TDownloading -> do
                    threadDelay $ 1000 * 1000
                    liftIO $ onLoopTick td st
                    loop
                _ -> return ()

onLoopTick :: TorrentDownload -> DownloadStrategy -> IO ()
onLoopTick td st = st td >>= mapM_ handleCommand
  where handleCommand :: DownloadCommand -> IO ()
        handleCommand = \case
            (DownloadPiece piece idx size) ->
                atomically $ modifyTVar
                    (tdPieceStatus td Vector.! idx)
                    flagPieceDownload
        flagPieceDownload = undefined
