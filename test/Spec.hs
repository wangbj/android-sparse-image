{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Bits as Bits
import           System.Android.SparseImage

data LL = LL ByteString deriving Show

instance Arbitrary LL where
  arbitrary = LL . LBS.pack . take 0x100000 <$> listOf arbitrary

data WellDefinedBlockSize = WellDefinedBlockSize Int deriving Show

alignUp :: Int -> Int -> Int
alignUp x y = (x + y - 1) Bits..&. (Bits.complement (y-1))

instance Arbitrary WellDefinedBlockSize where
  arbitrary = WellDefinedBlockSize . flip alignUp 4  . (+1024) . abs <$> arbitrary

prop_encode_decode_equals_id :: LL -> Bool
prop_encode_decode_equals_id (LL s) = s == decoded
  where
    encoded = encode s
    decoded = decode encoded

prop_encode_decode_equals_id_blockSize (LL s) (WellDefinedBlockSize bsize) = either (const False) id (encodeWithOpts opt s >>= \enc -> eitherDecode enc >>= \dec -> return (dec == s))
  where
    opt = defaultSparseOptions { sparseBlockSize = bsize }

return []
runQuickCheckTests = $quickCheckAll

main :: IO ()
main = runQuickCheckTests >> return ()
