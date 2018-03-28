module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy (ByteString)


import           Control.Applicative
import           Options.Applicative
import           Data.Semigroup

import           System.Android.SparseImage

data CommandLine = CommandLine String String

commandLineParser = CommandLine
              <$> argument str
                  ( metavar "IN"
                 <> help "image file" )
              <*> argument str
                  ( metavar "OUT"
                <> help "sparsed output file" )

commandOpts = info (commandLineParser <**> helper)
              ( fullDesc
              <> progDesc "convert image to sparse image"
              <> header "img2simg")

unsparseImage (CommandLine infile outfile) = LBS.readFile infile >>= \unsparsed ->
  LBS.writeFile outfile (getSparsed (encode unsparsed))

main :: IO ()
main = unsparseImage =<< execParser commandOpts
