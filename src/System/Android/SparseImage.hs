module System.Android.SparseImage (
    Sparsed (..)
  , encode
  , decode
  ) where

import qualified Data.Binary.Get as Binary.Get

import Data.ByteString.Lazy (ByteString)
import System.Android.SparseImage.Types
import System.Android.SparseImage.Instances
import System.Android.SparseImage.Internal

encode :: ByteString -> Sparsed
encode = Sparsed . toSparse

decode :: Sparsed -> ByteString
decode = Binary.Get.runGet getSparse . getSparsed
