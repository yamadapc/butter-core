{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.Tracker where

import Butter.Core.Torrent (FileInfo(..), Torrent(..), fromBEncode, toBEncode)
import Butter.Core.Util (urlEncodeVars)
import Data.BEncode as BE (BEncode, (.:), (.=!), (.=?), (<*>?), (<*>!),
                           (<$>!), decode, endDict, fromDict, toDict)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (toStrict)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import Data.Typeable (Typeable)
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

data TorrentStatus = TorrentStatus { tsTorrent :: Torrent
                                   , tsDownloaded :: Integer
                                   , tsUploaded :: Integer
                                   }

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

queryTracker :: B.ByteString -> Manager -> TorrentStatus -> IO TrackerResponse
queryTracker clientId m (TorrentStatus to d u) =
    case parseUrl $ C.unpack $ miAnnounce to of
        Nothing  -> fail "Invalid announce URL."
        Just req -> do
            let be = fiHash $ miInfo to
                req' = req { queryString = C.pack $ '?' :
                               urlEncodeVars [ ("info_hash" , C.unpack be)
                                             , ("peer_id"   , C.unpack clientId)
                                             , ("port"      , "3000")
                                             , ("uploaded"  , show u)
                                             , ("downloaded", show d)
                                             , ("compact"   , "1")
                                             , ("numwant"   , "50")
                                             , ("event"     , "started")
                                             ]
                           }

            res <- httpLbs req' m
            case BE.decode (L.toStrict (responseBody res)) of
                Left err   -> fail err
                Right tres -> return tres
