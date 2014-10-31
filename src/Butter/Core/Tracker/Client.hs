{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Butter.Core.Tracker.Client
-- Copyright   : Pedro Tacla Yamada
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Pedro Tacla Yamada <tacla.yamada@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A abstraction over a Tracker client, hitting some announce URL
-- continously.
module Butter.Core.Tracker.Client where

import Butter.Core.MetaInfo (FileInfo(..), MetaInfo(..), fromBEncode, toBEncode)
import Butter.Core.Peer as Peer (Peer, PeerId, decode)
import Butter.Core.Torrent
import Butter.Core.Util (urlEncodeVars)
import Control.Concurrent (Chan, forkIO, newChan, threadDelay, writeList2Chan)
import Control.Concurrent.STM (TVar, readTVarIO)
import Data.BEncode as BE (BEncode, (.:), (.=!), (.=?), (<*>?), (<*>!),
                           (<$>!), decode, endDict, fromDict, toDict)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (fromStrict, toStrict)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Network.HTTP.Client

data TrackerResponse = TrackerResponse { trComplete    :: Integer
                                       , trIncomplete  :: Integer
                                       , trInterval    :: Integer
                                       , trMinInterval :: Maybe Integer
                                       , trPeersString :: B.ByteString
                                       , trTrackerId   :: Maybe B.ByteString
                                       , trWarning     :: Maybe B.ByteString
                                       }
  deriving(Eq, Ord, Show, Typeable)

instance BE.BEncode TrackerResponse where
    toBEncode TrackerResponse {..} = toDict $ "complete"     .=! trComplete
                                           .: "incomplete"   .=! trIncomplete
                                           .: "interval"     .=! trInterval
                                           .: "min_interval" .=? trMinInterval
                                           .: "peers"        .=! trPeersString
                                           .: "tracker_id"   .=? trTrackerId
                                           .: "warning"      .=? trWarning
                                           .: endDict

    fromBEncode = fromDict $ TrackerResponse <$>! "complete"
                                             <*>! "incomplete"
                                             <*>! "interval"
                                             <*>? "min_interval"
                                             <*>! "peers"
                                             <*>? "tracker_id"
                                             <*>? "warning"

-- |
-- Gets a channel of peers, which will be fed as they come-in and
-- a TorrentStatus, which should be updated as the downloaded proceeds
getPeersChan :: Manager  -- ^ A HTTP manager
             -> PeerId   -- ^ The local peer's id
             -> Word16   -- ^ The port the local peer is listening at
             -> MetaInfo -- ^ A parsed torrent metainfo
             -> IO (TVar TorrentStatus, Chan Peer)
getPeersChan manager clientId p MetaInfo{..} = do
    tsVar <- newTStatusTVar
    c <- newChan :: IO (Chan Peer)
    _ <- forkIO $ do
        ts <- queryTracker' "start" 0 0
        updateChanAndWait ts c
        loop tsVar c

    return (tsVar, c)
  where ih = fiHash miInfo
        queryTracker' = queryTracker manager clientId (C.unpack miAnnounce) p ih
        updateChanAndWait TrackerResponse{..} c = do
            writeList2Chan c $ Peer.decode (L.fromStrict trPeersString)
            threadDelay $ fromIntegral trInterval * 1000
        loop tsVar c = readTVarIO tsVar >>= \case
            TorrentStatus TDownloading d u -> do -- Hit the Tracker with `update`
                ts <- queryTracker' "update" d u
                updateChanAndWait ts c
                loop tsVar c
            _ -> return () -- Torrent is done, stop querying

-- |
-- Queries an announce URL for peers
queryTracker :: Manager      -- ^ A HTTP manager
             -> PeerId       -- ^ The local peer's id
             -> String       -- ^ A tracker's announce URL
             -> Word16       -- ^ The port the local peer is listening at
             -> B.ByteString -- ^ A torrent's info hash
             -> String       -- ^ The event to send to the tracker
             -> Integer      -- ^ The amount of data already downloaded
             -> Integer      -- ^ The amount of data already uploaded
             -> IO TrackerResponse
queryTracker manager clientId announceUrl p infoHash evt downAmt upAmt =
    case parseUrl announceUrl of
        Nothing  -> fail "Invalid announce URL."
        Just req -> do
            let req' = req { queryString = C.pack $ '?' :
                               urlEncodeVars [ ("info_hash" , C.unpack infoHash)
                                             , ("peer_id"   , C.unpack clientId)
                                             , ("port"      , show p)
                                             , ("uploaded"  , show downAmt)
                                             , ("downloaded", show upAmt)
                                             , ("compact"   , "1")
                                             , ("numwant"   , "50")
                                             , ("event"     , evt)
                                             ]
                           }

            res <- httpLbs req' manager
            case BE.decode (L.toStrict (responseBody res)) of
                Left err   -> fail err
                Right tres -> return tres
