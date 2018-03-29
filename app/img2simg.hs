module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy (ByteString)


import           Control.Applicative
import           Options.Applicative
import           Data.Semigroup
import           Data.Maybe
import           System.Android.SparseImage

data CommandLine = CommandLine String String (Maybe Int)

commandLineParser = CommandLine
              <$> argument str
                  ( metavar "IN"
                 <> help "image file" )
              <*> argument str
                  ( metavar "OUT"
                <> help "sparsed output file" )
              <*> (optional $ argument auto $
                  ( metavar "BLOCK_SIZE"
                <> help "block size"))

commandOpts = info (commandLineParser <**> helper)
              ( fullDesc
              <> progDesc "convert image to sparse image"
              <> header "img2simg")

unsparseImage (CommandLine infile outfile blocksize_) = do
  unsparsed <- LBS.readFile infile
  let defaultBlockSize = sparseBlockSize defaultSparseOptions
      opts             = defaultSparseOptions { sparseBlockSize = fromMaybe defaultBlockSize blocksize_ }
  either print (LBS.writeFile outfile) (encodeWith opts unsparsed)

main :: IO ()
main = unsparseImage =<< execParser commandOpts
