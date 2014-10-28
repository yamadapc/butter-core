{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.Tracker.Client where

import Butter.Core.Torrent (FileInfo(..), Torrent(..), fromBEncode, toBEncode)
import Butter.Core.Peer as Peer (Peer, PeerId, decode)
import Butter.Core.Util (urlEncodeVars)
import Control.Concurrent
import Data.BEncode as BE (BEncode, (.:), (.=!), (.=?), (<*>?), (<*>!),
                           (<$>!), decode, endDict, fromDict, toDict)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (fromStrict, toStrict)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Data.Typeable (Typeable)

import Network.HTTP.Client

data TorrentStatus = Downloading { tsInfoHash   :: B.ByteString
                                 , tsDownloaded :: Integer
                                 , tsUploaded   :: Integer
                                 }
                   | Completed
  deriving(Eq, Ord, Show)

data TrackerClient = TrackerClient { tcAnnounceInterval :: Int
                                   , tcAnnounceUrl      :: B.ByteString
                                   , tcErrChan          :: Chan String
                                   , tcManager          :: Manager
                                   , tcPeersChan        :: Chan Peer
                                   }

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
-- Gets a channel of peers, which will be feeded as they come-in
getPeersChan :: Manager -> -- ^ An HTTP manager
                PeerId ->  -- ^ The local peer's id
                Torrent -> -- ^ A parsed torrent metainfo
                IO (Chan Peer)
getPeersChan manager clientId Torrent{..} = do
    chan <- newChan :: IO (Chan Peer)
    _ <- forkIO $ loop chan
    return chan
  where queryTracker' = queryTracker manager clientId (C.unpack miAnnounce)
                                     (fiHash miInfo) 0 0
        loop c = do
            TrackerResponse{..} <- queryTracker'
            writeList2Chan c $ Peer.decode (L.fromStrict trPeersString)
            threadDelay $ fromIntegral trInterval * 1000
            loop c


-- |
-- Queries an announce URL for peers
queryTracker :: Manager ->      -- ^ An HTTP manager
                PeerId ->       -- ^ The local peer's id
                String ->       -- ^ A tracker's announce URL
                B.ByteString -> -- ^ A torrent's info hash
                Integer ->      -- ^ The amount of data already downloaded
                Integer ->      -- ^ The amount of data already uploaded
                IO TrackerResponse
queryTracker manager clientId announceUrl infoHash downloadedAmt uploadedAmt =
    case parseUrl announceUrl of
        Nothing  -> fail "Invalid announce URL."
        Just req -> do
            let req' = req { queryString = C.pack $ '?' :
                               urlEncodeVars [ ("info_hash" , C.unpack infoHash)
                                             , ("peer_id"   , C.unpack clientId)
                                             , ("port"      , "3000")
                                             , ("uploaded"  , show downloadedAmt)
                                             , ("downloaded", show uploadedAmt)
                                             , ("compact"   , "1")
                                             , ("numwant"   , "50")
                                             , ("event"     , "started")
                                             ]
                           }

            res <- httpLbs req' manager
            case BE.decode (L.toStrict (responseBody res)) of
                Left err   -> fail err
                Right tres -> return tres
