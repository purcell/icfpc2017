{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Function (on)
import Data.List as List (intercalate, nubBy, permutations, sortBy)
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import GamePlay
import Lib (readMap)
import Offline (dumpState)
import Strategy
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

strategies :: [(String, Strategy)]
strategies =
  [ ("FirstUnclaimed", nextMoveFirstUnclaimed)
  , ("Heuristic", nextMove (defaultWeights {wOnShortestMinePath = 0}))
  , ("HeuristicWithMineShortestPath", nextMove defaultWeights)
  , ("MST", nextMoveMST)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mapFile, playerList, dumpGameNum, dumpFile] -> do
      let playerNames = wordsBy (== ',') playerList
      let selectedStrategies = filter ((`elem` playerNames) . fst) strategies
      let nDumpGame :: Int = read dumpGameNum
      when
        ((length selectedStrategies /= length playerNames) || null playerNames) $ do
        hPutStrLn stderr "Error: zero (or invalid) strategies specified."
        usageExit
      let lineups = nubBy ((==) `on` fmap fst) (permutations selectedStrategies)
      hPutStrLn stderr $
        "Testing " <> show (length lineups) <> " distinct lineups."
      let runLineUp ranks (gameNum, lineup) = do
            hPutStrLn stderr $
              "GAME " <> show gameNum <> ": " <>
              intercalate " => " (fst <$> lineup)
            results <- runGame mapFile lineup
            printResults results
            let curRanks =
                  M.fromList $
                  zip
                    (prName <$> sortBy (flip (comparing prScore)) results)
                    [0 ..]
            when (gameNum == nDumpGame) $
              dumpState dumpFile (prState (last results))
            hPutStrLn stderr "-----------------------"
            return $ M.unionWith (+) curRanks ranks
      let initialRanks = M.fromList $ (\(n, _) -> (n, 0)) <$> selectedStrategies
      finalRanks <- foldM runLineUp initialRanks (zip [1 ..] lineups)
      hPutStrLn stderr $
        "Overall ranks (lower is better): " <>
        show (sortBy (comparing snd) (M.toList finalRanks))
    _ -> usageExit

usageExit :: IO ()
usageExit =
  die $
  unlines
    [ "usage: evolver MAPFILE PLAYER1,PLAYER2,... DUMPGAMENUM DUMPFILE"
    , "Available players: " <> show (fst <$> strategies)
    ]

printResults :: [PlayerResult] -> IO ()
printResults results =
  forM_ (zip [1 ..] (sortBy (flip (comparing prScore)) results)) $ \(n, res) ->
    hPutStrLn stderr $
    show n <> ": " <> prName res <> " (" <> show (prScore res) <> ")"

data PlayerResult = PlayerResult
  { prName :: String
  , prState :: GameState
  , prScore :: Int
  }

runGame :: FilePath -> [(String, Strategy)] -> IO [PlayerResult]
runGame mapFile strats = do
  Right gameMap <- readMap mapFile
  let finalPlayers = last (simulate gameMap strats)
  return $
    (\(n, gs) -> PlayerResult n gs (scoreForThisPunter gs)) <$> finalPlayers
