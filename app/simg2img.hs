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
                 <> help "spase image file" )
              <*> argument str
                  ( metavar "OUT"
                <> help "unsparsed output file" )

commandOpts = info (commandLineParser <**> helper)
              ( fullDesc
              <> progDesc "convert sparse image to regular image"
              <> header "simg2img")

unsparseImage (CommandLine infile outfile) = LBS.readFile infile >>= \sparsed ->
  LBS.writeFile outfile (decode sparsed)

main :: IO ()
main = unsparseImage =<< execParser commandOpts
