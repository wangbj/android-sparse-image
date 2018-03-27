module System.Android.SparseImage.Instances where

#include "sparse_format.h"

import System.Android.SparseImage.Types

import Foreign.Storable
import Data.Binary

import System.Endian

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
            <$> (fromLE16 <$> #{peek chunk_header_t, chunk_type}   ptr)
            <*> (fromLE16 <$> #{peek chunk_header_t, reserved1}    ptr)
            <*> (fromLE32 <$> #{peek chunk_header_t, chunk_sz}     ptr)
            <*> (fromLE32 <$> #{peek chunk_header_t, total_sz}     ptr)
  poke ptr (ChunkHeader chunk_type reserved1 chunk_sz total_sz) =
       #{poke chunk_header_t, chunk_type}   ptr (toLE16 chunk_type)
    *> #{poke chunk_header_t, reserved1}    ptr (toLE16 reserved1)
    *> #{poke chunk_header_t, chunk_sz}     ptr (toLE32 chunk_sz)
    *> #{poke chunk_header_t, total_sz}     ptr (toLE32 total_sz)

getWord16le :: Get Word16
getWord32le :: Get Word32

getWord16le = fromLE16 <$> get
getWord32le = fromLE32 <$> get

putWord16le = put . toLE16
putWord32le = put . toLE32

instance Binary SparseHeader where
  get = SparseHeader
    <$> getWord32le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
  put (SparseHeader magic major minor hdrsz chunkhdrsz blksz totalblks totalchunks csum) =
       putWord32le magic
    *> putWord16le major
    *> putWord16le minor
    *> putWord16le hdrsz
    *> putWord16le chunkhdrsz
    *> putWord32le blksz
    *> putWord32le totalblks
    *> putWord32le totalchunks
    *> putWord32le csum

instance Binary ChunkHeader where
  get = ChunkHeader
    <$> getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
  put (ChunkHeader ty rsd csz tsz) =
       putWord16le ty
    *> putWord16le rsd
    *> putWord32le csz
    *> putWord32le tsz
