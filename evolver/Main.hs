{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (readMap)
import GamePlay
import Offline (writeStateToFileUnconditionally)
import Visualiser (visualiseFromDump)

main :: IO ()
main = do
  -- Read a map in
  Right gameMap <- readMap "./examples/circle.json"
  -- Generate our initial state for the game
  let setup = SetupState 0 1 gameMap
  let gameState = precomputeGameState setup
  -- Iterate over some turns
  let updatedState = iterate takeTurn gameState !! 10
  writeStateToFileUnconditionally updatedState
  visualiseFromDump
  return ()

takeTurn :: GameState -> GameState
takeTurn state = updateState [fst $ nextMove state] (snd $ nextMove state)
