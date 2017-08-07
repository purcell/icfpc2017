{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GamePlay where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Foldable (concatMap, sum)
import qualified Data.Graph.Inductive.Basic as GB
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as GPT
import qualified Data.Graph.Inductive.Query.BFS as BFS
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Prelude hiding (map)
import Types hiding (id)

data GameState = GameState
  { initialState :: SetupState
  , prevMoves :: [Move]
  , shortestMinePaths :: [Set River]
  } deriving (Generic, FromJSON, ToJSON)

data SetupState = SetupState
  { punter :: PunterID
  , punters :: Int
  , map :: Map
  } deriving (Generic, FromJSON, ToJSON)

data Claim = Claim
  { claimPunter :: PunterID
  , claimRiver :: River
  } deriving (Eq, Ord, Show)

claims :: GameState -> Set Claim
claims = claimsInMoves . prevMoves

claimsInMoves :: [Move] -> Set Claim
claimsInMoves =
  S.fromList .
  concatMap
    (\m ->
       case m of
         ClaimMove p r -> pure (Claim p r)
         Pass _ -> mempty
         Option p r -> pure (Claim p r)
         Splurge p path -> Claim p <$> S.toList (pathToRivers path))

moveInits :: GameState -> [[Move]]
moveInits gameState = List.inits $ prevMoves gameState

precomputeGameState :: SetupState -> GameState
precomputeGameState s = GameState s [] minePaths
  where
    ms = S.toList $ mines (GamePlay.map s)
    g = graph (GamePlay.map s)
    minePaths =
      [pathToRivers (BFS.esp m m2 g) | (m:rest) <- List.tails ms, m2 <- rest]

optionsRemaining :: GameState -> PunterID -> Int
optionsRemaining s p = allowance - length (filter wasOpt (prevMoves s))
  where
    wasOpt (Option p' _)
      | p' == p = True
    wasOpt _ = False
    allowance = S.size $ (mines . map . initialState) s

pathToRivers :: [SiteID] -> Set River
pathToRivers nodes = S.fromList $ zipWith River nodes (tail nodes)

updateState :: [Move] -> GameState -> GameState
updateState moves s = s {prevMoves = prevMoves s <> moves}

myPunterID :: GameState -> PunterID
myPunterID s = GamePlay.punter (initialState s)

puntersInGame :: GameState -> S.Set PunterID
puntersInGame state = S.fromList $ List.map punterIDFromMove $ prevMoves state

punterIDFromMove :: Move -> PunterID
punterIDFromMove (Pass p) = p
punterIDFromMove (ClaimMove p _) = p
punterIDFromMove (Splurge p _) = p
punterIDFromMove (Option p _) = p

data Strategy a = Strategy
  { strategyParams :: a
  , strategyMove :: a -> GameState -> (Move, GameState)
  }

data Player a = Player
  { playerStrategy :: Strategy a
  , playerState :: GameState
  }

strategyApply :: Strategy a -> GameState -> (Move, GameState)
strategyApply s = strategyMove s (strategyParams s)

playerScore :: Player a -> Int
playerScore p = scoreForPunter (playerState p) (playerPunter p)

playerPunter :: Player a -> PunterID
playerPunter = GamePlay.punter . initialState . playerState

simulate :: Map -> [Strategy a] -> [Player a]
simulate theMap strategies =
  let numPlayers = length strategies
      players = uncurry makePlayer <$> zip strategies [1 ..]
      makePlayer s n =
        let initState = precomputeGameState (SetupState n numPlayers theMap)
        in Player s initState
      numTurns = S.size (rivers theMap)
      playRound (cur:others) =
        let (move, newPS) = strategyApply (playerStrategy cur) (playerState cur)
            newCur = cur {playerState = newPS}
        in (\p -> p {playerState = updateState [move] (playerState p)}) <$>
           (others ++ [newCur])
      playRound [] = []
  in iterate playRound players !! numTurns

connectedTo :: River -> SiteID -> Bool
connectedTo (River s t) site = s == site || t == site

progress :: GameState -> (Int, Int)
progress s =
  (length (prevMoves s), (S.size . rivers . GamePlay.map . initialState) s)

riversToSites :: Set River -> Set SiteID
riversToSites = foldr combine S.empty
  where
    combine (River s t) ss = ss `S.union` S.fromList [s, t]

claimants :: Map -> [Move] -> M.Map River [PunterID]
claimants m mvs =
  M.unionWith
    (++)
    (M.fromList
       ((\c -> (claimRiver c, [claimPunter c])) <$> S.toList (claimsInMoves mvs)))
    (M.fromList ((, []) <$> S.toList (rivers m)))

graph :: Map -> GPT.Gr SiteID River
graph = graphOfRivers . rivers

graphOfRivers :: Set River -> GPT.Gr SiteID River
graphOfRivers rs = GB.undir $ G.mkGraph nodes edges
  where
    nodes = (id &&& id) <$> S.toList (riversToSites rs)
    edges = (\r@(River s t) -> (s, t, r)) <$> S.toList rs

scoreForPunter :: GameState -> PunterID -> Int
scoreForPunter gs p =
  score
    (GamePlay.map (initialState gs))
    (S.map claimRiver (S.filter (\c -> claimPunter c == p) (claims gs)))

score :: Map -> Set River -> Int
score mp rs =
  sum [shortestDist m s ^ 2 | m <- S.toList (mines mp), s <- reachableSites m]
  where
    fullGraph = graph mp
    claimGraph = graphOfRivers rs
    reachableSites m = BFS.bfs m claimGraph
    shortestDist m s = length $ tail $ BFS.esp m s fullGraph
