module System.Android.SparseImage (
    encode
  , encodeWith
  , decode
  , eitherDecode
  , defaultSparseOptions
  , SparseOptions (..)
  , SparseImageError (..)
  ) where

import qualified Data.Binary.Get as Binary.Get

import Data.ByteString.Lazy (ByteString)
import System.Android.SparseImage.Types
import System.Android.SparseImage.Instances
import System.Android.SparseImage.Internal

encode :: ByteString -> ByteString
encode = either (error . show) id . encodeWith defaultSparseOptions

encodeWith :: SparseOptions -> ByteString -> Either SparseImageError ByteString
encodeWith opt = sparseWith opt

runGet = Binary.Get.runGet

eitherDecode :: ByteString -> Either SparseImageError ByteString
eitherDecode = runGet sparseGetter

decode = either (error . show) id . eitherDecode
