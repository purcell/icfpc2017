{-# LANGUAGE ScopedTypeVariables #-}
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
              [ ("Original", nextMove defaultWeights)
              , ( "Hybrid"
                , nextMoveMST (defaultWeights {wOnShortestMinePath = 0}))
              ]
      let finalPlayers :: [(String, GameState)] =
            last (simulate gameMap (take (read numPlayers) strategies))
          (_, finalState) = last finalPlayers
      let playersWithScores =
            (zip
               finalPlayers
               ((\(_, gs) -> scoreForThisPunter gs) <$> finalPlayers))
      forM_ (sortBy (comparing snd) playersWithScores) $ \((name, gs), sc) ->
        hPutStrLn stderr $
        "Player " <> show (punter (initialState gs)) <> ", strategy=" <>
        show name <>
        ", score=" <>
        show sc
      dumpState dumpFile finalState
    _ -> die "usage: evolver MAPFILE DUMPFILE NUMPLAYERS"
