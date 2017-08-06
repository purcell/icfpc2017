{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set as S
import GamePlay
import Lib (readMap)
import Offline (dumpState)
import System.Environment (getArgs)
import System.Exit
import Types

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mapFile, dumpFile, numPlayers]
       -- Read a map in
     -> do
      Right gameMap <- readMap mapFile
      let turns = S.size (sites gameMap) `div` read numPlayers
       -- Generate our initial state for the game
      let setup = SetupState 0 1 gameMap
      let gameState = precomputeGameState setup
       -- Iterate over some turns
      let updatedState = iterate moveAndUpdate gameState !! turns
      dumpState dumpFile updatedState
      return ()
    _ -> die "usage: evolver MAPFILE DUMPFILE NUMPLAYERS"
