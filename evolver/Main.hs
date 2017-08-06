{-# LANGUAGE OverloadedStrings #-}

module Main where

import GamePlay
import Lib (readMap)
import Offline (dumpState)
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mapFile, dumpFile, numPlayers]
       -- Read a map in
     -> do
      Right gameMap <- readMap mapFile
      let finalPlayers = simulate gameMap (replicate (read numPlayers) nextMove)
      let finalState = playerState $ head finalPlayers
      dumpState dumpFile finalState
      return ()
    _ -> die "usage: evolver MAPFILE DUMPFILE NUMPLAYERS"
