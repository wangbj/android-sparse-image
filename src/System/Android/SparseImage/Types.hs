module System.Android.SparseImage.Types (
    LE64
  , LE32
  , LE16
  , sparseHeaderMagicNum
  , SparseHeader (..)
  , ChunkHeader (..) ) where

import Data.Word

type LE64 = Word64
type LE32 = Word32
type LE16 = Word16

data SparseHeader = SparseHeader {
    shMagicNum        :: LE32
  , shMajorVersion    :: LE16
  , shMinorVersion    :: LE16
  , shFileHeaderSize  :: LE16
  , shChunkHeaderSize :: LE16
  , shBlockSize       :: LE32
  , shTotalBlocks     :: LE32
  , shTotalChunks     :: LE32
  , shImageCheckSum   :: LE32
  } deriving (Show, Eq)

sparseHeaderMagicNum :: LE32
sparseHeaderMagicNum = 0xed26ff3a

data ChunkHeader = ChunkHeader {
    chChunkType :: LE16
  , chReserved1 :: LE16
  , chChunkSize :: LE32
  , chTotalSize :: LE32
  } deriving (Show, Eq)
