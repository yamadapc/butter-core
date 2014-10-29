-- |
-- Module      : Butter.Core.Torrent
-- Copyright   : Pedro Tacla Yamada
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Pedro Tacla Yamada <tacla.yamada@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A high-level abstraction over a Torrent download.
module Butter.Core.Torrent where

import Control.Concurrent.STM (TVar, newTVarIO)

data TorrentStage = TStopped | TDownloading | TCompleted
  deriving(Eq, Ord, Show)

data TorrentStatus = TorrentStatus { tStage      :: TorrentStage
                                   , tDownloaded :: Integer
                                   , tUploaded   :: Integer
                                   }
  deriving(Eq, Ord, Show)

newTStatusTVar :: IO (TVar TorrentStatus)
newTStatusTVar = newTVarIO $ TorrentStatus TDownloading 0 0
