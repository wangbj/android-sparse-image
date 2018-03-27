module System.Android.SparseImage.Types (
    LE64 (..)
  , LE32 (..)
  , LE16 (..)
  , sparseHeaderMagicNum
  , SparseHeader (..)
  , ChunkHeader (..) ) where

import Data.Word

import System.Endian

newtype LE64 = LE64 { __le64 :: Word64 } deriving (Show, Read, Eq, Ord)
newtype LE32 = LE32 { __le32 :: Word32 } deriving (Show, Read, Eq, Ord)
newtype LE16 = LE16 { __le16 :: Word16 } deriving (Show, Read, Eq, Ord)

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
sparseHeaderMagicNum = LE32 (toLE32 0xed26ff3a)

data ChunkHeader = ChunkHeader {
    chChunkType :: LE16
  , chReserved1 :: LE16
  , chChunkSize :: LE32
  , chTotalSize :: LE32
  } deriving (Show, Eq)
