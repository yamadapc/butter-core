{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Butter.Core.MetaInfo where

import Data.BEncode as BE
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)

data MetaInfo = MetaInfo { miAnnounce     :: !B.ByteString
                         , miAnnounceList :: !(Maybe [[B.ByteString]])
                         , miComment      :: !(Maybe B.ByteString)
                         , miCreatedBy    :: !(Maybe B.ByteString)
                         , miCreationDate :: !(Maybe Integer)
                         , miEncoding     :: !(Maybe B.ByteString)
                         , miInfo         :: !FileInfo
                         }
  deriving(Eq, Typeable, Show)

data FileInfo = FileInfo { fiPieceLength :: !Integer
                         , fiPieces      :: !B.ByteString
                         , fiPrivate     :: !(Maybe Integer)

                         , fiName        :: !(Maybe B.ByteString)
                         , fiLength      :: !(Maybe Integer)

                         , fiMd5Sum      :: !(Maybe B.ByteString)
                         , fiFiles       :: !(Maybe [FileNode])
                         }
  deriving(Eq, Typeable, Show)

data FileNode = FileNode { fnLength :: !Integer
                         , fnMd5Sum :: !(Maybe B.ByteString)
                         , fnPath   :: !B.ByteString
                         }
  deriving(Eq, Typeable, Show)

instance BE.BEncode MetaInfo where
    toBEncode MetaInfo {..} = toDict $ "announce"      .=! miAnnounce
                                    .: "announce-list" .=? miAnnounceList
                                    .: "comment"       .=? miComment
                                    .: "created by"    .=? miCreatedBy
                                    .: "creation date" .=? miCreationDate
                                    .: "encoding"      .=? miEncoding
                                    .: "info"          .=! miInfo
                                    .: endDict

    fromBEncode = fromDict $ MetaInfo <$>! "announce"
                                      <*>? "announce-list"
                                      <*>? "comment"
                                      <*>? "created by"
                                      <*>? "creation date"
                                      <*>? "encoding"
                                      <*>! "info"

instance BE.BEncode FileInfo where
    toBEncode FileInfo {..} = toDict $ "piece length"  .=! fiPieceLength
                                    .: "pieces"        .=! fiPieces
                                    .: "private"       .=? fiPrivate
                                    .: "name"          .=? fiName
                                    .: "length"        .=? fiLength
                                    .: "md5sum"        .=? fiMd5Sum
                                    .: "files"         .=? fiFiles
                                    .: endDict

    fromBEncode = fromDict $ FileInfo <$>! "piece length"
                                      <*>! "pieces"
                                      <*>? "private"
                                      <*>? "name"
                                      <*>? "length"
                                      <*>? "md5sum"
                                      <*>? "files"

instance BE.BEncode FileNode where
    toBEncode FileNode {..} = toDict $ "length" .=! fnLength
                                    .: "md5sum" .=? fnMd5Sum
                                    .: "path"   .=! fnPath
                                    .: endDict

    fromBEncode = fromDict $ FileNode <$>! "length"
                                      <*>? "md5sum"
                                      <*>! "path"
