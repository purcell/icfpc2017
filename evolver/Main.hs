{-# LANGUAGE OverloadedStrings #-}

module Main where

import GamePlay
import Lib (readMap)
import Offline (dumpState)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mapFile, dumpFile]
       -- Read a map in
     -> do
      Right gameMap <- readMap mapFile
       -- Generate our initial state for the game
      let setup = SetupState 0 1 gameMap
      let gameState = precomputeGameState setup
       -- Iterate over some turns
      let updatedState = iterate moveAndUpdate gameState !! 10
      dumpState dumpFile updatedState
      return ()
    _ -> error "usage: evolver MAPFILE DUMPFILE"
