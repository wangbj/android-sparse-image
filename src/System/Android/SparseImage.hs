module System.Android.SparseImage (
    encode
  , decode
  , eitherDecode
  ) where

import qualified Data.Binary.Get as Binary.Get

import Data.ByteString.Lazy (ByteString)
import System.Android.SparseImage.Types
import System.Android.SparseImage.Instances
import System.Android.SparseImage.Internal

encode :: ByteString -> ByteString
encode = toSparse

runGet = Binary.Get.runGet

eitherDecode :: ByteString -> Either SparseImageError ByteString
eitherDecode = runGet sparseGetter

decode = either (error . show) id . eitherDecode

