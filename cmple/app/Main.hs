{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import ArgParser (Args(..), parseArgs)
import Mode (execMode)

main :: IO ()
main = do
  args <- parseArgs
  execMode $ mode args