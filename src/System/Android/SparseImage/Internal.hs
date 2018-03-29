{-# LANGUAGE LambdaCase #-}

module System.Android.SparseImage.Internal (
    sparseGetter
  , sparseWith
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteArray as BA
import           Data.ByteArray (ByteArray, Bytes)
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed (Vector, (!))
import           Data.Maybe
import           Data.Word
import           Data.Int
import           Data.Bits
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Semigroup
import           Control.Monad.Reader
import           Control.Applicative
import           Foreign.Storable
import           Text.Printf
import           System.Endian
import           System.Android.SparseImage.Types
import           System.Android.SparseImage.Instances
import           System.Android.SparseImage.Crc32

sparseHeaderSize, chunkHeaderSize :: Int
sparseHeaderSize = sizeOf (undefined :: SparseHeader)
chunkHeaderSize = sizeOf (undefined :: ChunkHeader)

word32Size :: Int
word32Size = sizeOf (undefined :: Word32)

getChunkDataHelper :: Int -> Maybe Crc32 -> ChunkHeader -> Get (Either SparseImageError (Maybe Crc32, LBS.ByteString))
getChunkDataHelper blockSize crcIn (ChunkHeader chunkType _ chunkSize totalSize)
  | chunkType == ChunkRaw      = (getLazyByteString (fromIntegral chunkDataSize)) >>= \lbs -> return . Right $ (doCrc32 crcIn lbs, lbs)
  | chunkType == ChunkFill     = ( (toLazyByteString . mconcat . replicate (realChunkSize `div` word32Size) . word32LE) <$> getWord32le )>>= \lbs -> return . Right $ (doCrc32 crcIn lbs, lbs)
  | chunkType == ChunkDontCare = (skip chunkDataSize) >> (return $ Right (doCrc32 crcIn dnc, mempty))
  | chunkType == ChunkCrc32    = getWord32host >>= \crc -> if isNothing crcIn || Just (Crc32 crc) == crcIn
    then return $ Right (crcIn, mempty)
    else return $ Left (SparseImageBadCrc crc (maybe 0 getCrc32 crcIn))
  where
    chunkDataSize = fromIntegral totalSize - (fromIntegral chunkHeaderSize) :: Int
    realChunkSize = (fromIntegral chunkSize) * blockSize :: Int
    dnc           = LBS.replicate (fromIntegral realChunkSize) 0

consRB :: Monad m => (a, LBS.ByteString) -> m (a, Builder) -> m (a, Builder)
consRB (_, x) m = liftM (\(c', x') -> (c', lazyByteString x <> x')) m

getChunks blockSize crc = do
  eof <- isEmpty
  if eof then return (Right (crc, mempty)) else do
    ch <- get :: Get ChunkHeader
    curr <- getChunkDataHelper blockSize crc ch
    case curr of
      Left err ->          return (Left err)
      Right (crc', lbs) -> liftM (consRB (crc', lbs)) (getChunks blockSize crc')

sparseGetterWithCrc crc = do
  sparseHeader <- get :: Get SparseHeader
  getChunks (fromIntegral (shBlockSize sparseHeader)) crc >>= \case
    Left err -> return (Left err)
    Right (crc', b) -> return (Right (crc', toLazyByteString b))

sparseGetter = sparseGetterWithCrc Nothing >>= return . fmap snd

data BlockData = BlockDataRaw Builder Int64
               | BlockDataFill Word32 Int64

isFiller bs = cmpNext bs1 bs'
  where
    (bs1, bs') = S.splitAt word32Size bs
    cmpNext c s
      | S.null s = True
      | c == s1  = cmpNext c s'
      | c /= s1  = False
      where
        (s1, s') = S.splitAt word32Size s
      
toBlocks :: Int -> LBS.ByteString -> [ByteString]
toBlocks blockSize lbs = if S.null curr then [] else (curr:) (toBlocks blockSize rest)
  where
    (currL, rest) = LBS.splitAt (fromIntegral blockSize) lbs
    curr          = LBS.toStrict currL

toBlockDataHelper blockSize curr
  | S.length curr  < blockSize = BlockDataRaw (byteString curr) (fromIntegral (S.length curr))
  | S.length curr >= blockSize = if (not . isFiller) curr
    then (BlockDataRaw (byteString curr) (fromIntegral blockSize))
    else (BlockDataFill filler (fromIntegral blockSize))
  where
    filler        = runGet getWord32host (LBS.fromStrict curr)

mergeBlocks :: [BlockData] -> [BlockData]
mergeBlocks [] = []
mergeBlocks [x] = [x]
mergeBlocks (BlockDataRaw bs s:BlockDataFill fill n:xs) = (BlockDataRaw bs s) : mergeBlocks (BlockDataFill fill n:xs)
mergeBlocks (BlockDataFill fill n : BlockDataRaw bs s : xs) = (BlockDataFill fill n): mergeBlocks (BlockDataRaw bs s: xs)
mergeBlocks (BlockDataRaw bs s: BlockDataRaw bs' s' : xs) = mergeBlocks (BlockDataRaw (bs <> bs') (s + s') : xs)
mergeBlocks (BlockDataFill fill1 n1:BlockDataFill fill2 n2:xs)
  | fill1 == fill2 = mergeBlocks (BlockDataFill fill1 (n1+n2) : xs)
  | fill1 /= fill2 = (BlockDataFill fill1 n1) : mergeBlocks (BlockDataFill fill2 n2 : xs)

mkChunkHeader :: Int -> BlockData -> ChunkHeader
mkChunkHeader blockSize (BlockDataRaw _ n) = ChunkHeader ChunkRaw 0 ( (toLE32 . fromIntegral) blocks) ( (toLE32 . fromIntegral) total)
  where
    blocks = (n + fromIntegral blockSize - 1) `div` fromIntegral blockSize
    total  = n + fromIntegral chunkHeaderSize
mkChunkHeader blockSize (BlockDataFill _ n) = ChunkHeader ChunkFill 0 ( (toLE32 . fromIntegral) blocks) ( (toLE32 . fromIntegral) total)
  where
    blocks = (n + fromIntegral blockSize - 1) `div` fromIntegral blockSize
    total  = word32Size + chunkHeaderSize

encodeBlockData :: Int -> BlockData -> (Builder, Int64)
encodeBlockData blockSize blk@(BlockDataRaw b n) = (lazyByteString (encode chunkHeader) <> b, n)
  where
    chunkHeader = mkChunkHeader blockSize blk
encodeBlockData blockSize blk@(BlockDataFill fill n) = (lazyByteString (encode chunkHeader <> filler), n)
  where
    chunkHeader = mkChunkHeader blockSize blk
    filler      = runPut (putWord32host fill)

encodeChunks :: Int -> [(Builder, Int64)] -> LBS.ByteString
encodeChunks blockSize = encodeAllChunks . foldl go (0, 0, mempty)
  where
    go (totalSize, totalChunks, s) (b, n) = (totalSize+n, totalChunks+1, s <> b)
    go :: (Int64, Int, Builder) -> (Builder, Int64) -> (Int64, Int, Builder)
    encodeAllChunks (totalSize, totalChunks, s) = toLazyByteString (lazyByteString (encode sparseHeader) <> s)
      where
        totalBlocks = (totalSize+fromIntegral blockSize-1) `div` fromIntegral blockSize
        sparseHeader = SparseHeader ( (toLE32 . fromIntegral) sparseHeaderMagic) (toLE16 1) (toLE16 0) ( (toLE16 . fromIntegral) sparseHeaderSize) ( (toLE16 . fromIntegral) chunkHeaderSize) ( (toLE32 . fromIntegral) blockSize) ( (toLE32 . fromIntegral) totalBlocks) ( (toLE32 .  fromIntegral) totalChunks) (toLE32 0)

sparseHelper :: Int -> LBS.ByteString -> Either SparseImageError LBS.ByteString
sparseHelper blockSize bs
  | blockSize < 1024 || blockSize `mod` 4 /= 0 = Left (SparseImageInvalidBlockSize blockSize)
  | otherwise = Right . encodeChunks blockSize . map (encodeBlockData blockSize) . mergeBlocks . map (toBlockDataHelper blockSize) . toBlocks blockSize $ bs

sparseWith :: SparseOptions -> LBS.ByteString -> Either SparseImageError LBS.ByteString
sparseWith opt = sparseHelper (sparseBlockSize opt)
