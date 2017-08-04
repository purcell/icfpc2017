module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  [file] <- getArgs
  result <- readMap file
  print result
