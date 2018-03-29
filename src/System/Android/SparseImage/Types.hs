{-# LANGUAGE ExistentialQuantification #-}

module System.Android.SparseImage.Types (
    LE64
  , LE32
  , LE16
  , SparseHeader (..)
  , SparseImageError (..)
  , ChunkType (..)
  , Crc32 (..)
  , SparseOptions (..)
  , defaultSparseOptions
  , ChunkHeader (..) ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Control.Exception (Exception)
import           Data.Word
import           Text.Printf
import           Data.List (intercalate)
import           Data.Semigroup

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

data ChunkHeader = ChunkHeader {
    chChunkType :: ChunkType
  , chReserved1 :: LE16
  , chChunkSize :: LE32
  , chTotalSize :: LE32
  } deriving (Show, Eq)

data ChunkType = ChunkRaw
               | ChunkFill
               | ChunkDontCare
               | ChunkCrc32
               deriving (Show, Read, Eq, Ord)

data SparseImageError = SparseImageInvalidBlockSize Int
                      | SparseImageInvalidChunkSize Int
                      | SparseImageBadCrc Word32 Word32

instance Show SparseImageError where
  show (SparseImageInvalidBlockSize blockSize) = "SparseImageInvalidBlockSize: " ++ show blockSize
  show (SparseImageInvalidChunkSize chunkSize) = "SparseImageInvalidChunkSize: " ++ show chunkSize
  show (SparseImageBadCrc expected actual) = "SparseImageBadCrc: expected: " ++ printf "%x" expected ++ ", actual: " ++ printf "%x" actual

instance Exception SparseImageError

newtype Crc32 = Crc32 { getCrc32 :: Word32 } deriving (Show, Read, Eq, Ord)

data SparseOptions = SparseOptions {
    sparseBlockSize :: Int
  } deriving (Eq, Show)

defaultBlockSize :: Int
defaultBlockSize = 4096

defaultSparseOptions = SparseOptions { sparseBlockSize = defaultBlockSize }
