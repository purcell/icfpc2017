{-# LANGUAGE OverloadedStrings #-}

module RunVisualisation where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy as BSL (readFile)
import Data.Semigroup ((<>))
import Visualiser (writeStateToSvgFiles)

main :: IO ()
main = do
  dumpedState <- BSL.readFile "dumps/state.json"
  case eitherDecode dumpedState of
    (Right state) -> do
      putStrLn "Successfully decoded state from /dumps/state.json"
      fileCount <- writeStateToSvgFiles state
      putStrLn $ "Wrote " <> show fileCount <> " svg files to /visualisations"
    (Left err) ->
      error $ "Couldn't write svg files, error decoding dumped state: " <> err
