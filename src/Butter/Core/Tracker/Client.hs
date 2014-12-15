{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- A abstraction over a Tracker client, hitting some announce URL.
--
-- Incomming peer addresses are modeled using a 'TBQueue' and the client stops
-- asking for peers if the queue gets full.
--
module Butter.Core.Tracker.Client ( -- * Subscribing for peers
                                    TrackerClient
                                  , startTrackerClient
                                  , stopTrackerClient
                                  , completeTrackerClient
                                    -- * Low-level querying
                                  , queryTracker
                                  , queryTracker'
                                  , loopAnnounceUpdate
                                  , pushAddrs
                                  , waitInterval
                                  , resolveInterval
                                  , TrackerResponse(..)
                                  )
  where

import Butter.Core.MetaInfo (InfoHash, fromBEncode, toBEncode)
import Butter.Core.PeerWire as PeerWire (PeerAddr, PeerId, decode)
import Butter.Core.Torrent -- (TorrentDownload(..), TorrentStatus(..), TorrentStage(..))
import Butter.Core.Util (urlEncodeVars, writeList2TBQueueIO)
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TBQueue, TVar, newTBQueueIO, readTVarIO,
                               atomically, isFullTBQueue)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.BEncode as BE (BEncode, (.:), (.=!), (.=?), (<*>?), (<*>!), (<$>!),
                           decode, endDict, fromDict, toDict)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (fromStrict, toStrict)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Data.Typeable (Typeable)
import Network.Socket (PortNumber(..))
import Network.HTTP.Client (Manager, httpLbs, parseUrl, responseBody,
                            queryString)

-- * Subscribing for peers
-------------------------------------------------------------------------------

-- |
-- Represents a tracker client, encapsulating the infinite loop around
-- tracker announce queries
type TrackerClient = (ThreadId, TBQueue PeerAddr)

-- |
-- Starts a tracker client's loop
startTrackerClient :: (MonadIO m, MonadReader TorrentDownload m)
                   => m TrackerClient
startTrackerClient = do
    td <- ask
    let opts = tdOptions td
        st   = tdStatus td

    liftIO $ do
        q <- newTBQueueIO (cLimit opts)
        tid <- forkIO $ do
            tr <- queryTracker' opts (cNumwant opts) "started" 0 0
            pushAddrs q tr
            waitInterval opts tr
            loopAnnounceUpdate opts st q
        return (tid, q)

-- |
-- Stops the looping 'TrackerClient' thread and sends the tracker
-- a @"stopped"@ event.
stopTrackerClient :: (MonadIO m, MonadReader TorrentDownload m)
                  => TrackerClient
                  -> m ()
stopTrackerClient (tid, _) = do
    td <- ask

    liftIO $ void $ do
        killThread tid
        TorrentStatus _ d u <- readTVarIO (tdStatus td)
        queryTracker' (tdOptions td) 0 "stopped" d u

-- |
-- Sends the @"completed" event to the tracker.
completeTrackerClient :: (MonadIO m, MonadReader TorrentDownload m)
                      => m ()
completeTrackerClient = do
    td <- ask
    liftIO $ void $ queryTracker' (tdOptions td) 0 "completed" 100 100


-- * Low-level querying
-------------------------------------------------------------------------------

-- |
-- Queries an announce URL for peers. This function is not meant for client
-- use, but rather as an internal helper for this module. The number of
-- parameters is a little tasteless, but there's no need to have the cost
-- of a ADT here.
queryTracker :: Manager    -- ^ A HTTP manager
             -> PeerId     -- ^ The local peer's id
             -> String     -- ^ A tracker's announce URL
             -> PortNumber -- ^ The port the local peer is listening at
             -> InfoHash   -- ^ A torrent's info hash
             -> Integer    -- ^ How many peers to ask the tracker for
             -> String     -- ^ The event to send to the tracker
             -> Integer    -- ^ The amount of data already downloaded
             -> Integer    -- ^ The amount of data already uploaded
             -> IO TrackerResponse
queryTracker manager clientId announceUrl p infoHash numwant evt downAmt upAmt =
    let PortNum p' = p in
    case parseUrl announceUrl of
        Nothing  -> fail "Invalid announce URL."
        Just req -> do
            let req' = req { queryString = C.pack $ '?' :
                               urlEncodeVars [ ("info_hash" , C.unpack infoHash)
                                             , ("peer_id"   , C.unpack clientId)
                                             , ("port"      , show p')
                                             , ("uploaded"  , show downAmt)
                                             , ("downloaded", show upAmt)
                                             , ("compact"   , "1")
                                             , ("numwant"   , show numwant)
                                             , ("event"     , evt)
                                             ]
                           }

            res <- httpLbs req' manager
            case BE.decode (L.toStrict (responseBody res)) of
                Left err   -> fail err
                Right tres -> return tres

-- |
-- Helper for using 'queryTracker' with 'TrackerClientOptions'
queryTracker' :: TrackerClientOptions -> Integer -> String -> Integer -> Integer
              -> IO TrackerResponse
queryTracker' TrackerClientOptions{..} =
    queryTracker cManager cPeerId cAnnounceUrl cPort cInfoHash

-- |
-- Starts an infinite loop, which keeps hitting the tracker announce URL
-- and feeding a 'TBQueue' with new peers.
loopAnnounceUpdate :: TrackerClientOptions -> TVar TorrentStatus
                   -> TBQueue PeerAddr -> IO ()
loopAnnounceUpdate opts tsVar q = readTVarIO tsVar >>= \case
    -- Hit the Tracker with `update` when the torrent is
    -- downloading
    TorrentStatus TDownloading d u -> do
        f <- atomically $ isFullTBQueue q
        -- Don't ask for peers if the queue is full
        tr <- let n = if f then 0 else cNumwant opts
              in queryTracker' opts n "update" d u
        pushAddrs q tr >> waitInterval opts tr
        loopAnnounceUpdate opts tsVar q
    -- Otherwise just stop
    _ -> return ()

-- |
-- Pushes new 'PeerAddr's into the output queue
pushAddrs :: TBQueue PeerAddr -> TrackerResponse -> IO ()
pushAddrs q tr = writeList2TBQueueIO q peerAddrs
  where peersStringL = L.fromStrict $ trPeersString tr
        peerAddrs = PeerWire.decode peersStringL

-- |
-- Resolves and waits between hits to the tracker
waitInterval :: TrackerClientOptions -> TrackerResponse -> IO ()
waitInterval opts tr = threadDelay $ 1000000 * resolveInterval opts tr

-- |
-- Resolves the interval that needs to be waited between hits to the tracker
resolveInterval :: TrackerClientOptions -> TrackerResponse -> Int
resolveInterval opts tr = case cInterval opts of
    Just i -> maybe i (\m -> if m > i then m else i)
                    (fromInteger <$> trMinInterval tr)
    Nothing -> fromInteger $ trInterval tr


-- |
-- The data type for a tracker's announce response.
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
