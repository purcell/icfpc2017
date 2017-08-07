{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy as BSL (readFile)
import Data.Semigroup ((<>))
import System.Environment (getArgs)
import System.Exit
import Visualiser (writeStateToSvgFiles)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      dumpedState <- BSL.readFile fileName
      case eitherDecode dumpedState of
        (Right state) -> do
          putStrLn $ "Successfully decoded state from " <> fileName
          fileCount <- writeStateToSvgFiles state
          putStrLn $
            "Wrote " <> show fileCount <> " svg files to /visualisations"
        (Left err) ->
          error $
          "Couldn't write svg files, error decoding dumped state: " <> err
    _ -> die "usage: prog dump-file-name"
