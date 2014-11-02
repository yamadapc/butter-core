{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Butter.Core.MetaInfo
-- Copyright   : Pedro Tacla Yamada
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Pedro Tacla Yamada <tacla.yamada@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Models serialization and deserialization from bencoded data to parsed
-- data. Currently uses the @bencoding@ package to do that, simply defining
-- a couple of data structures and 'BEncode' instances. This module
-- shouldn't contain logic beyond this format interface.
--
-- There are plans of making this unnecessary by extending the @bencoding@
-- package with template haskell features from @aeson@, but it's what
-- currently works
module Butter.Core.MetaInfo ( FileInfo(..)
                            , FileNode(..)
                            , MetaInfo(..)
                            , fromBEncode
                            , readMetaInfoFile
                            , toBEncode
                            -- Exported from `Data.BEncode`
                            , decode
                            , encode
                            ) where

import Control.Applicative ((<*>), pure)
import qualified Crypto.Hash.SHA1 as SHA1 (hashlazy)
import Data.BEncode as BE
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString, readFile)

-- |
-- Represents a parsed @.torrent@ file. Should be used as a middle-man between
-- the torrent file and a higher-level data structure
data MetaInfo = MetaInfo { miAnnounce     :: !B.ByteString
                         , miAnnounceList :: !(Maybe [[B.ByteString]])
                         , miComment      :: !(Maybe B.ByteString)
                         , miCreatedBy    :: !(Maybe B.ByteString)
                         , miCreationDate :: !(Maybe Integer)
                         , miEncoding     :: !(Maybe B.ByteString)
                         , miInfo         :: !FileInfo
                         }
  deriving(Eq, Show, Typeable)

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

-- |
-- Convenience function for reading and parsing a a torent's metainfo from
-- a file.
readMetaInfoFile :: FilePath -> IO MetaInfo
readMetaInfoFile fp = fmap BE.decode (B.readFile fp) >>= \case
    Left err -> fail err
    Right to -> return to

-- |
-- A torrent's @"info"@ node.
data FileInfo = FileInfo { fiFiles       :: !(Maybe [FileNode])
                         , fiLength      :: !(Maybe Integer)
                         , fiMd5Sum      :: !(Maybe B.ByteString)
                         , fiName        :: !(Maybe B.ByteString)
                         , fiPieceLength :: !Integer
                         , fiPieces      :: !B.ByteString
                         , fiPrivate     :: !(Maybe Bool)
                         , fiHash        :: !B.ByteString
                         }
  deriving(Eq, Typeable, Show)

instance BE.BEncode FileInfo where
    toBEncode FileInfo {..} = toDict $ "files"        .=? fiFiles
                                    .: "length"       .=? fiLength
                                    .: "md5sum"       .=? fiMd5Sum
                                    .: "name"         .=? fiName
                                    .: "piece length" .=! fiPieceLength
                                    .: "pieces"       .=! fiPieces
                                    .: "private"      .=? fiPrivate
                                    .: endDict

    fromBEncode d = let hash = SHA1.hashlazy $ BE.encode d in
                      fromDict (FileInfo <$>? "files"
                                         <*>? "length"
                                         <*>? "md5sum"
                                         <*>? "name"
                                         <*>! "piece length"
                                         <*>! "pieces"
                                         <*>? "private"
                                         <*> pure hash) d

-- |
-- A @"info"@ node's @"files"@ node element.
data FileNode = FileNode { fnLength :: !Integer
                         , fnMd5Sum :: !(Maybe B.ByteString)
                         , fnPath   :: !B.ByteString
                         }
  deriving(Eq, Typeable, Show)


instance BE.BEncode FileNode where
    toBEncode FileNode {..} = toDict $ "length" .=! fnLength
                                    .: "md5sum" .=? fnMd5Sum
                                    .: "path"   .=! fnPath
                                    .: endDict

    fromBEncode = fromDict $ FileNode <$>! "length"
                                      <*>? "md5sum"
                                      <*>! "path"
