module System.Android.SparseImage.Instances where

#include "sparse_format.h"

import System.Android.SparseImage.Types

import Foreign.Storable
import Foreign.Ptr
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import System.Endian

sparseHeaderMagic :: LE32
sparseHeaderMagic = fromIntegral (#const SPARSE_HEADER_MAGIC)

chunkTypeRaw, chunkTypeFill, chunkTypeDontCare, chunkTypeCrc32 :: LE16
chunkTypeRaw      = fromIntegral (#const CHUNK_TYPE_RAW)
chunkTypeFill     = fromIntegral (#const CHUNK_TYPE_FILL)
chunkTypeDontCare = fromIntegral (#const CHUNK_TYPE_DONT_CARE)
chunkTypeCrc32    = fromIntegral (#const CHUNK_TYPE_CRC32)

getIf :: (a -> Bool) -> String -> Get a -> Get a
getIf p msg getter = getter >>= \x -> if p x then return x else fail msg

putIf :: (a -> Bool) -> String -> a -> (a -> Put) -> Put
putIf p msg a putter = if p a then putter a else fail msg

expect :: (Eq a, Show a) => a -> Get a -> Get a
expect a getter = getIf (==a) ("expect: " ++ show a) getter

oneOf :: (Eq a, Show a) => [a] -> Get a -> Get a
oneOf xs getter = getIf (\x -> x `elem` xs) ("expect: oneOf " ++ show xs) getter

instance Storable SparseHeader where
  sizeOf    _ = #{size sparse_header_t}
  alignment _ = #{alignment sparse_header_t}
  peek ptr    = SparseHeader
            <$> (fromLE32 <$> #{peek sparse_header_t, magic}          ptr)
            <*> (fromLE16 <$> #{peek sparse_header_t, major_version}  ptr)
            <*> (fromLE16 <$> #{peek sparse_header_t, minor_version}  ptr)
            <*> (fromLE16 <$> #{peek sparse_header_t, file_hdr_sz}    ptr)
            <*> (fromLE16 <$> #{peek sparse_header_t, chunk_hdr_sz}   ptr)
            <*> (fromLE32 <$> #{peek sparse_header_t, blk_sz}         ptr)
            <*> (fromLE32 <$> #{peek sparse_header_t, total_blks}     ptr)
            <*> (fromLE32 <$> #{peek sparse_header_t, total_chunks}   ptr)
            <*> (fromLE32 <$> #{peek sparse_header_t, image_checksum} ptr)
  poke ptr (SparseHeader magic major_version minor_version file_hdr_sz chunk_hdr_sz blk_sz total_blks total_chunks image_checksum) =
       #{poke sparse_header_t, magic}           ptr (toLE32 magic)
    *> #{poke sparse_header_t, major_version}   ptr (toLE16 major_version)
    *> #{poke sparse_header_t, minor_version}   ptr (toLE16 minor_version)
    *> #{poke sparse_header_t, file_hdr_sz}     ptr (toLE16 file_hdr_sz)
    *> #{poke sparse_header_t, chunk_hdr_sz}    ptr (toLE16 chunk_hdr_sz)
    *> #{poke sparse_header_t, blk_sz}          ptr (toLE32 blk_sz)
    *> #{poke sparse_header_t, total_blks}      ptr (toLE32 total_blks)
    *> #{poke sparse_header_t, total_chunks}    ptr (toLE32 total_chunks)
    *> #{poke sparse_header_t, image_checksum}  ptr (toLE32 image_checksum)

instance Storable ChunkHeader where
  sizeOf    _ = #{size chunk_header_t}
  alignment _ = #{alignment chunk_header_t}
  peek ptr    = ChunkHeader
            <$> ((toChunkType . fromLE16) <$> #{peek chunk_header_t, chunk_type}   ptr)
            <*> (fromLE16 <$> #{peek chunk_header_t, reserved1}    ptr)
            <*> (fromLE32 <$> #{peek chunk_header_t, chunk_sz}     ptr)
            <*> (fromLE32 <$> #{peek chunk_header_t, total_sz}     ptr)
  poke ptr (ChunkHeader chunk_type reserved1 chunk_sz total_sz) =
       #{poke chunk_header_t, chunk_type}   ptr ((toLE16 . fromChunkType) chunk_type)
    *> #{poke chunk_header_t, reserved1}    ptr (toLE16 reserved1)
    *> #{poke chunk_header_t, chunk_sz}     ptr (toLE32 chunk_sz)
    *> #{poke chunk_header_t, total_sz}     ptr (toLE32 total_sz)

sparseHeaderSize, chunkHeaderSize :: Int
sparseHeaderSize = sizeOf (undefined :: SparseHeader)
chunkHeaderSize  = sizeOf (undefined :: ChunkHeader)

instance Binary SparseHeader where
  get = SparseHeader
    <$> expect sparseHeaderMagic getWord32le
    <*> expect 1 getWord16le
    <*> expect 0 getWord16le
    <*> expect (fromIntegral sparseHeaderSize) getWord16le
    <*> expect (fromIntegral chunkHeaderSize)  getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
  put (SparseHeader magic major minor hdrsz chunkhdrsz blksz totalblks totalchunks csum) =
       (pure (magic == sparseHeaderMagic)) *> putWord32le magic
    *> (pure (major == 1)) *> putWord16le major
    *> (pure (minor == 0)) *> putWord16le minor
    *> (pure (fromIntegral hdrsz == sparseHeaderSize)) *> putWord16le hdrsz
    *> (pure (fromIntegral chunkhdrsz == chunkHeaderSize)) *> putWord16le chunkhdrsz
    *> putWord32le blksz
    *> putWord32le totalblks
    *> putWord32le totalchunks
    *> putWord32le csum

toChunkType :: LE16 -> ChunkType
toChunkType v
  | v == chunkTypeRaw      = ChunkRaw
  | v == chunkTypeFill     = ChunkFill
  | v == chunkTypeDontCare = ChunkDontCare
  | v == chunkTypeCrc32    = ChunkCrc32
  | otherwise              = error $ "toChunkType: " ++ show v

fromChunkType :: ChunkType -> LE16
fromChunkType ChunkRaw      = chunkTypeRaw
fromChunkType ChunkFill     = chunkTypeFill
fromChunkType ChunkDontCare = chunkTypeDontCare
fromChunkType ChunkCrc32    = chunkTypeCrc32

validChunkTypes = [chunkTypeRaw, chunkTypeFill, chunkTypeDontCare, chunkTypeCrc32]

instance Binary ChunkHeader where
  get = ChunkHeader
    <$> (toChunkType <$> (oneOf validChunkTypes getWord16le))
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
  put (ChunkHeader ty rsd csz tsz) =
       putWord16le (fromChunkType ty)
    *> putWord16le rsd
    *> putWord32le csz
    *> putWord32le tsz
