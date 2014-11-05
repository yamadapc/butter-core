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
-- continously. For most use cases, using 'startTrackerClientSimple' should be
-- enough.
--
-- Incomming peer addresses are modeled using a 'TBQueue' and the client stops
-- asking for peers if the queue gets full.
--
-- > TrackerClient _ clientTid peersQ torrentStatus <- startTrackerClient'
-- > peerAddr <- readTBQueue peersQ
-- > -- Do things with the peer address
-- > -- ...
-- > -- Possibly update the torrentStatusVar 'TVar'
-- > modifyTVar torrentStatus (\ts -> ts { tDownloaded = tDownloaded ts + 10 })
-- > -- ...
-- > -- Kill the tracker client if cancellation is necessary
-- > killThread clientTid
module Butter.Core.Tracker.Client (
                                  -- * Subscribing for peers
                                    TrackerClient(..)
                                  , startTrackerClientSimple
                                  , clientOptions
                                  , ClientOptions(..)
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

import Butter.Core.MetaInfo (FileInfo(..), InfoHash, MetaInfo(..), fromBEncode,
                             toBEncode)
import Butter.Core.PeerWire as PeerWire (PeerAddr, PeerId, decode)
import Butter.Core.Torrent (TorrentStatus(..), TorrentStage(..), newTStatusTVar)
import Butter.Core.Util (urlEncodeVars, writeList2TBQueueIO)
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TBQueue, TVar, newTBQueueIO, readTVarIO,
                               atomically, isFullTBQueue)
import Control.Monad (void)
import Data.BEncode as BE (BEncode, (.:), (.=!), (.=?), (<*>?), (<*>!),
                           (<$>!), decode, endDict, fromDict, toDict)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (fromStrict, toStrict)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Data.Typeable (Typeable)
import Network.Socket (PortNumber(..))
import Network.HTTP.Client (Manager, httpLbs, parseUrl, responseBody,
                            queryString)

-- |
-- Represents a tracker client, encapsulating the infinite loop around
-- tracker announce queries
data TrackerClient = TrackerClient { tcOptions :: ClientOptions
                                   , tcTid     :: ThreadId
                                   , tcPeersQ  :: TBQueue PeerAddr
                                   , tcTStatus :: TVar TorrentStatus
                                   }

-- |
-- Options for tracker announce querying
data ClientOptions = ClientOptions { cManager :: Manager
                                   -- ^ An @HTTP.Client@ 'Manager'
                                   , cPeerId :: PeerId
                                   -- ^ The local peer's id
                                   , cPort :: PortNumber
                                   -- ^ The port the local peer is
                                   -- listening at
                                   , cAnnounceUrl :: String
                                   -- ^ The announce URL to hit
                                   , cInfoHash :: B.ByteString
                                   -- ^ The "info_hash" to query for
                                   , cInterval :: Maybe Int
                                   -- ^ The interval between queries in
                                   -- seconds. Will use the interval
                                   -- suggested by the tracker if set to
                                   -- @Nothing@. Will be ignored if bigger
                                   -- than the tracker's minimum
                                   , cNumwant :: Integer
                                   -- ^ The number of peers to ask for, on
                                   -- each query
                                   , cLimit :: Int
                                   -- ^ The limit of peers to ask for see
                                   -- 'startTrackerClient' for more
                                   -- information
                                   }

-- |
-- For most use cases, using this helper function should give you sensible
-- defaults for the 'ClientOptions'
clientOptions :: Manager -> PeerId -> PortNumber -> MetaInfo -> ClientOptions
clientOptions manager peerId port metaInfo =
    ClientOptions manager peerId port announce hash Nothing 30 100
  where announce = C.unpack $ miAnnounce metaInfo
        hash = fiHash $ miInfo metaInfo

-- |
-- Shorthand for using 'clientOptions' composed with 'startTrackerClient'
startTrackerClientSimple :: Manager -> PeerId -> PortNumber -> MetaInfo
                         -> IO TrackerClient
startTrackerClientSimple m p n mi = startTrackerClient $ clientOptions m p n mi


-- |
-- Starts an infinite loop for querying the tracker announce URL, with the
-- given client options.
--
-- The TrackerClient ADT contains the ThreadId for the spawned worker
-- thread, a 'TVar' containing the current torrent download status (which
-- needs to be updated by the client) and a 'TBQueue' bounded queue which
-- will be fed with new peer addresses as they come in, limitted according
-- to the @ClientOptions@.
startTrackerClient :: ClientOptions -- ^ The options to start the client with
                   -> IO TrackerClient
startTrackerClient opts = do
    tsVar <- newTStatusTVar
    q <- newTBQueueIO (cLimit opts)
    tid <- forkIO $ do
        tr <- queryTracker' opts (cNumwant opts) "start" 0 0
        pushAddrs q tr
        waitInterval opts tr
        loopAnnounceUpdate opts tsVar q

    return $ TrackerClient opts tid q tsVar

-- |
-- Stops the looping 'TrackerClient' thread and sends the tracker
-- a @"stopped"@ event.
stopTrackerClient :: TrackerClient -> IO ()
stopTrackerClient tc = do
    TorrentStatus _ d u <- readTVarIO (tcTStatus tc)
    killThread (tcTid tc)
    void $ queryTracker' (tcOptions tc) 0 "stopped" d u

-- |
-- Sends the @"completed" event to the tracker.
completeTrackerClient :: TrackerClient -> IO ()
completeTrackerClient tc =
    void $ queryTracker' (tcOptions tc) 0 "completed" 100 100

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
-- Helper for using 'queryTracker' with 'ClientOptions'
queryTracker' :: ClientOptions -> Integer -> String -> Integer -> Integer
              -> IO TrackerResponse
queryTracker' ClientOptions{..} =
    queryTracker cManager cPeerId cAnnounceUrl cPort cInfoHash

-- |
-- Starts an infinite loop, which keeps hitting the tracker announce URL
-- and feeding a 'TBQueue' with new peers.
loopAnnounceUpdate :: ClientOptions -> TVar TorrentStatus -> TBQueue PeerAddr
                   -> IO ()
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
waitInterval :: ClientOptions -> TrackerResponse -> IO ()
waitInterval opts tr = threadDelay $ 1000000 * resolveInterval opts tr

-- |
-- Resolves the interval that needs to be waited between hits to the tracker
resolveInterval :: ClientOptions -> TrackerResponse -> Int
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
