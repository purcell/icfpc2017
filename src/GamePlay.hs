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
  { claimPunter :: !PunterID
  , claimRiver :: !River
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
         Splurge p path -> Claim p <$> pathToRivers path)

moveInits :: GameState -> [[Move]]
moveInits gameState = List.inits $ prevMoves gameState

precomputeGameState :: SetupState -> GameState
precomputeGameState s = GameState s [] minePaths
  where
    ms = S.toList $ mines (GamePlay.map s)
    g = graph (GamePlay.map s)
    minePaths =
      [ S.fromList $ pathToRivers (BFS.esp m m2 g)
      | (m:rest) <- List.tails ms
      , m2 <- rest
      ]

optionsRemaining :: Map -> [Move] -> PunterID -> Int
optionsRemaining m moves p = allowance - length (filter wasOpt moves)
  where
    wasOpt (Option p' _)
      | p' == p = True
    wasOpt _ = False
    allowance = S.size (mines m)

pathToRivers :: [SiteID] -> [River]
pathToRivers nodes = zipWith River nodes (tail nodes)

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

type Strategy = GameState -> (Move, GameState)

-- | Run the labelled strategies against each other on a given map and return the
-- evolving gameplay.
simulate :: Map -> [(a, Strategy)] -> [[(a, GameState)]]
simulate theMap namedStrategies =
  let numPlayers = length namedStrategies
      players =
        (\((name, strat), n) ->
           (name, strat, precomputeGameState (SetupState n numPlayers theMap))) <$>
        zip namedStrategies [0 .. (numPlayers - 1)]
      numTurns = S.size (rivers theMap)
      playRound ((name, strat, state):others) =
        let (move, state') = strat state
        in (\(nm, str, sta) -> (nm, str, updateState [move] sta)) <$>
           (others ++ [(name, strat, state')])
      playRound [] = []
  in fmap (\(nm, _, sta) -> (nm, sta)) <$>
     take numTurns (iterate playRound players)

connectedTo :: River -> SiteID -> Bool
connectedTo (River s t) site = s == site || t == site

progress :: GameState -> (Int, Int)
progress s =
  (length (prevMoves s), (S.size . rivers . GamePlay.map . initialState) s)

riversToSites :: Set River -> Set SiteID
riversToSites = foldr combine S.empty
  where
    combine (River s t) ss = ss `S.union` S.fromList [s, t]

type Claimants = M.Map River [PunterID]

claimants :: Map -> [Move] -> Claimants
claimants m mvs =
  M.unionWith
    (++)
    (M.fromList
       ((\c -> (claimRiver c, [claimPunter c])) <$> S.toList (claimsInMoves mvs)))
    (M.fromList ((, []) <$> S.toList (rivers m)))

data ClaimState
  = AlreadyClaimed
  | Unclaimed
  | Optionable
  | Unavailable
  deriving (Eq)

claimable :: ClaimState -> Bool
claimable Unclaimed = True
claimable Optionable = True
claimable _ = False

potentiallyNavigable :: ClaimState -> Bool
potentiallyNavigable AlreadyClaimed = True
potentiallyNavigable c = claimable c

claimStates :: GameState -> M.Map River ClaimState
claimStates gs = claimState <$> riverClaims
  where
    mp = map (initialState gs)
    moves = prevMoves gs
    punterID = myPunterID gs
    riverClaims = claimants mp moves
    optionsLeft = optionsRemaining mp moves punterID
    claimState ps
      | punterID `elem` ps = AlreadyClaimed
    claimState [] = Unclaimed
    claimState [_]
      | optionsLeft > 0 = Optionable
    claimState _ = Unavailable

filterRiversByClaim ::
     M.Map River ClaimState -> (ClaimState -> Bool) -> Set River
filterRiversByClaim riverClaims f =
  S.fromList [r | (r, c) <- M.toList riverClaims, f c]

graph :: Map -> GPT.Gr SiteID River
graph = graphOfRivers . rivers

graphOfRivers :: Set River -> GPT.Gr SiteID River
graphOfRivers rs = GB.undir $ G.mkGraph nodes edges
  where
    nodes = (id &&& id) <$> S.toList (riversToSites rs)
    edges = (\r@(River s t) -> (s, t, r)) <$> S.toList rs

scoreForThisPunter :: GameState -> Int
scoreForThisPunter gs = scoreForPunter gs (myPunterID gs)

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
