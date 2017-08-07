{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.List as List (sortBy)
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import GamePlay
import Lib (readMap)
import Offline (dumpState)
import Strategy
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
      let strategies =
            cycle
              [ Strategy defaultWeights nextMove
              , Strategy (defaultWeights {wOnShortestMinePath = 0}) nextMoveMST
              ]
      let finalPlayers = simulate gameMap (take (read numPlayers) strategies)
          player = last finalPlayers
          finalState = playerState player
      let playersWithScores = (zip finalPlayers (playerScore <$> finalPlayers))
      forM_ (sortBy (comparing snd) playersWithScores) $ \(p, sc) ->
        hPutStrLn stderr $
        "Player " <> show (playerPunter p) <> ", weights " <>
        show (strategyParams (playerStrategy p)) <>
        ", score " <>
        show sc
      dumpState dumpFile finalState
    _ -> die "usage: evolver MAPFILE DUMPFILE NUMPLAYERS"
