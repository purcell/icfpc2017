{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GamePlay where

import Data.Aeson
import Data.Foldable (concatMap, sum)
import qualified Data.Graph.Inductive.Basic as GB
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as GPT
import qualified Data.Graph.Inductive.Query.BFS as BFS
import qualified Data.List as List
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Types

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

graph :: Map -> GPT.UGr
graph = graphOfRivers . rivers

graphOfRivers :: Set River -> GPT.UGr
graphOfRivers rs = GB.undir $ G.mkUGraph nodes edges
  where
    nodes = S.toList $ foldr combine S.empty rs
    combine :: River -> Set SiteID -> Set SiteID
    combine (River s t) ss = ss `S.union` S.fromList [s, t]
    edges = (\(River s t) -> (s, t)) <$> S.toList rs

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
