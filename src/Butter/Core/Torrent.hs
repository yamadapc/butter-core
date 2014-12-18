{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Butter.Core.Torrent
-- Copyright   : Pedro Tacla Yamada
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Pedro Tacla Yamada <tacla.yamada@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- The main `Torrent` data type
module Butter.Core.Torrent where

import Butter.Core.Block
import Butter.Core.MetaInfo
import Butter.Core.Peer
import Butter.Core.PeerWire
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM -- (TVar, newTVarIO, readTVarIO)
import Control.Monad.IO.Class
import qualified Data.ByteString as ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.Map as Map (Map, empty)
import qualified Data.Vector as Vector -- ((!))
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Socket (PortNumber(..))

data TorrentStage = TStopped | TDownloading | TCompleted
  deriving(Eq, Ord, Show)

data TorrentStatus = TorrentStatus { tsStage :: TorrentStage
                                   , tsDownloaded :: Integer
                                   , tsUploaded :: Integer
                                   }
  deriving(Eq, Ord, Show)

-- |
-- Options for tracker announce querying
data TrackerClientOptions =
    TrackerClientOptions { cManager :: Manager
                         -- ^ An @HTTP.Client@ 'Manager'
                         , cPeerId :: PeerId
                         -- ^ The local peer's id
                         , cPort :: PortNumber
                         -- ^ The port the local peer is listening at
                         , cAnnounceUrl :: String
                         -- ^ The announce URL to hit
                         , cInfoHash :: ByteString.ByteString
                         -- ^ The "info_hash" to query for
                         , cInterval :: Maybe Int
                         -- ^ The interval between queries in seconds. Will use
                         -- the interval -- suggested by the tracker if set to
                         -- @Nothing@. Will be ignored if bigger than the
                         -- tracker's minimum
                         , cNumwant :: Integer
                         -- ^ The number of peers to ask for, on each query
                         , cLimit :: Int
                         -- ^ The limit of peers to ask for see
                         -- 'startTrackerClient' for more information
                         }

data TorrentDownload =
    TorrentDownload { tdOptions :: TrackerClientOptions
                    , tdMetaInfo :: MetaInfo
                    , tdStatus :: TVar TorrentStatus
                    , tdPeers :: TVar (Map.Map PeerId Peer)
                    , tdPeersState :: TVar (Map.Map PeerId PeerState)
                    , tdPieceStatus :: Vector.Vector (TVar PieceStatus)
                    }

data PeerState = PeerState { psConnection :: TChan PeerMessage
                           }

data PieceStatus = PieceStatus { pPeersHave :: [PeerId]
                               , pBlocks :: Map.Map BlockId (PeerId, BlockStatus)
                               , pCompleted :: Bool
                               }

data BlockStatus = BSDownloading ThreadId
                 | BSErrored
                 | BSDone

newDownloadFromFile :: MonadIO m => FilePath -> m TorrentDownload
newDownloadFromFile fp = do
    (mi, pid, st, m1, m2) <- liftIO $ (,,,,) <$> readMetaInfoFile fp
                                             <*> newPeerId
                                             <*> newTStatusTVar
                                             <*> newTVarIO Map.empty
                                             <*> newTVarIO Map.empty

    opts <- clientOptions pid (PortNum 3000) mi
    return $ TorrentDownload opts mi st m1 m2 Vector.empty

-- |
-- For most use cases, using this helper function should give you sensible
-- defaults for the 'ClientOptions'
clientOptions :: MonadIO m => PeerId -> PortNumber -> MetaInfo
              -> m TrackerClientOptions
clientOptions pid port metaInfo = do
    m <- liftIO $ newManager defaultManagerSettings
    return $ TrackerClientOptions m pid port announce hash Nothing 30 100
  where announce = Char8.unpack $ miAnnounce metaInfo
        hash = fiHash $ miInfo metaInfo

newTStatusTVar :: IO (TVar TorrentStatus)
newTStatusTVar = newTVarIO $ TorrentStatus TDownloading 0 0
