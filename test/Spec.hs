{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy (ByteString)

import           System.Android.SparseImage

data LL = LL ByteString deriving Show

instance Arbitrary LL where
  arbitrary = LL . LBS.pack . take 0x100000 <$> listOf arbitrary

prop_encode_decode_equals_id :: LL -> Bool
prop_encode_decode_equals_id (LL s) = s == decoded
  where
    encoded = encode s
    decoded = decode encoded

return []
runQuickCheckTests = $quickCheckAll

main :: IO ()
main = runQuickCheckTests >> return ()
