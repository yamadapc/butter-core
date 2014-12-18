module Butter.Core.Block where

import Butter.Core.PeerWire (PWInteger)
import qualified Data.ByteString (ByteString)

data BlockId = BlockId { bidPieceIdx :: PWInteger
                       , bidBegin :: PWInteger
                       , length :: PWInteger
                       }
  deriving(Eq, Ord, Show)

type BlockSize = Integer
type BlockBody = Data.ByteString.ByteString
