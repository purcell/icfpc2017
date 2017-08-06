{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import GamePlay
import Lib (readMap)
import Offline (dumpState)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mapFile, dumpFile, numPlayers]
       -- Read a map in
     -> do
      Right gameMap <- readMap mapFile
      let finalPlayers = simulate gameMap (replicate (read numPlayers) nextMove)
      let player = last finalPlayers
          finalState = playerState player
      dumpState dumpFile finalState
      hPutStrLn stderr $ "Scores are " <> show (playerScore <$> finalPlayers)
      return ()
    _ -> die "usage: evolver MAPFILE DUMPFILE NUMPLAYERS"
