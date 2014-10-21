{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.Torrent ( FileInfo(..)
                           , FileNode(..)
                           , Torrent(..)
                           , fromBEncode
                           , infoHash
                           , toBEncode
                           ) where

import qualified Crypto.Hash.SHA1 as SHA1 (hashlazy)
import Data.BEncode as BE
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)

data Torrent = Torrent { miAnnounce     :: !B.ByteString
                       , miAnnounceList :: !(Maybe [[B.ByteString]])
                       , miComment      :: !(Maybe B.ByteString)
                       , miCreatedBy    :: !(Maybe B.ByteString)
                       , miCreationDate :: !(Maybe Integer)
                       , miEncoding     :: !(Maybe B.ByteString)
                       , miInfo         :: !FileInfo
                       }
  deriving(Eq, Typeable, Show)

data FileInfo = FileInfo { fiFiles       :: !(Maybe [FileNode])
                         , fiLength      :: !(Maybe Integer)
                         , fiMd5Sum      :: !(Maybe B.ByteString)
                         , fiName        :: !(Maybe B.ByteString)
                         , fiPieceLength :: !Integer
                         , fiPieces      :: !B.ByteString
                         , fiPrivate     :: !(Maybe Integer)

                         }
  deriving(Eq, Typeable, Show)

data FileNode = FileNode { fnLength :: !Integer
                         , fnMd5Sum :: !(Maybe B.ByteString)
                         , fnPath   :: !B.ByteString
                         }
  deriving(Eq, Typeable, Show)

instance BE.BEncode Torrent where
    toBEncode Torrent {..} = toDict $ "announce"      .=! miAnnounce
                                   .: "announce-list" .=? miAnnounceList
                                   .: "comment"       .=? miComment
                                   .: "created by"    .=? miCreatedBy
                                   .: "creation date" .=? miCreationDate
                                   .: "encoding"      .=? miEncoding
                                   .: "info"          .=! miInfo
                                   .: endDict

    fromBEncode = fromDict $ Torrent <$>! "announce"
                                     <*>? "announce-list"
                                     <*>? "comment"
                                     <*>? "created by"
                                     <*>? "creation date"
                                     <*>? "encoding"
                                     <*>! "info"

instance BE.BEncode FileInfo where
    toBEncode FileInfo {..} = toDict $ "files"        .=? fiFiles
                                    .: "length"       .=? fiLength
                                    .: "md5sum"       .=? fiMd5Sum
                                    .: "name"         .=? fiName
                                    .: "piece length" .=! fiPieceLength
                                    .: "pieces"       .=! fiPieces
                                    .: "private"      .=? fiPrivate
                                    .: endDict

    fromBEncode = fromDict $ FileInfo <$>? "files"
                                      <*>? "length"
                                      <*>? "md5sum"
                                      <*>? "name"
                                      <*>! "piece length"
                                      <*>! "pieces"
                                      <*>? "private"

instance BE.BEncode FileNode where
    toBEncode FileNode {..} = toDict $ "length" .=! fnLength
                                    .: "md5sum" .=? fnMd5Sum
                                    .: "path"   .=! fnPath
                                    .: endDict

    fromBEncode = fromDict $ FileNode <$>! "length"
                                      <*>? "md5sum"
                                      <*>! "path"

infoHash :: Torrent -> B.ByteString
infoHash = SHA1.hashlazy . BE.encode . miInfo
