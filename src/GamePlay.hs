{-# LANGUAGE TupleSections #-}
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
import Data.List as List (inits, map, sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import Data.Set (Set, (\\))
import qualified Data.Set as S
import GHC.Generics (Generic)
import Types

data GameState = GameState
  { initialState :: SetupState
  , prevMoves :: [Move]
  } deriving (Generic, FromJSON, ToJSON)

data SetupState = SetupState
  { punter :: PunterID
  , punters :: Int
  , map :: Map
  } deriving (Generic, FromJSON, ToJSON)

claims :: GameState -> Set Claim
claims = claimsInMoves . prevMoves

claimsInMoves :: [Move] -> Set Claim
claimsInMoves =
  S.fromList .
  concatMap
    (\m ->
       case m of
         ClaimMove c -> pure c
         Pass _ -> mempty)

moveInits :: GameState -> [[Move]]
moveInits gameState = inits $ prevMoves gameState

precomputeGameState :: SetupState -> GameState
precomputeGameState s = GameState s []

updateState :: [Move] -> GameState -> GameState
updateState moves s = s {prevMoves = prevMoves s <> moves}

myPunterID :: GameState -> PunterID
myPunterID s = GamePlay.punter (initialState s)

puntersInGame :: GameState -> S.Set PunterID
puntersInGame state = S.fromList $ List.map punterIDFromMove $ prevMoves state

punterIDFromMove :: Move -> PunterID
punterIDFromMove (Pass punterID) = punterID
punterIDFromMove (ClaimMove claim) = Types.punter claim

nextMove :: GameState -> (Move, GameState)
nextMove s = (ClaimMove bestClaim, s)
  where
    bestClaim = Claim (myPunterID s) (bestUnclaimedRiver s)

type Strategy = GameState -> (Move, GameState)

data Player = Player
  { playerStrategy :: Strategy
  , playerState :: GameState
  }

playerScore :: Player -> Int
playerScore p = scoreForPunter (playerState p) (playerPunter p)

playerPunter :: Player -> PunterID
playerPunter = GamePlay.punter . initialState . playerState

simulate :: Map -> [Strategy] -> [Player]
simulate theMap strategies =
  let numPlayers = length strategies
      players = uncurry makePlayer <$> zip strategies [1 ..]
      makePlayer s n =
        let initState = precomputeGameState (SetupState n numPlayers theMap)
        in Player s initState
      numTurns = S.size (rivers theMap)
      playRound :: [Player] -> [Player]
      playRound (cur:others) =
        let (move, newPS) = playerStrategy cur (playerState cur)
            newCur = cur {playerState = newPS}
        in (\p -> p {playerState = updateState [move] (playerState p)}) <$>
           (others ++ [newCur])
      playRound [] = []
  in iterate playRound players !! numTurns

connectedTo :: River -> SiteID -> Bool
connectedTo (River s t) site = s == site || t == site

bestUnclaimedRiver :: GameState -> River
bestUnclaimedRiver s =
  case listToMaybe $ sortBy (flip $ comparing riverScore) unclaimedRivers of
    Just r -> r
    _ -> error "how could there not be an unclaimed river?"
  where
    theMap = GamePlay.map (initialState s)
    unclaimedRivers = S.toList $ allRivers \\ claimedRivers
    allRivers = rivers theMap
    claimedRivers = S.map river (claims s)
    mySites :: Set SiteID
    mySites =
      S.fromList $
      concatMap
        (\c ->
           if (myPunterID s == Types.punter c)
             then [source (river c), target (river c)]
             else [])
        (S.toList (claims s))
    mineSites :: Set SiteID
    mineSites = mines theMap
    riverScore :: River -> Int
    riverScore r =
      (1 + sum (connectedMineScore <$> connectedMineSites r)) *
      (30 ^ endsTouchingMyRivers r)
    endsTouchingMyRivers :: River -> Int
    endsTouchingMyRivers r = S.size (S.filter (connectedTo r) mySites)
    connectedMineSites :: River -> [SiteID]
    connectedMineSites r = S.toList $ S.filter (connectedTo r) mineSites
    connectedMineScore :: SiteID -> Int
    connectedMineScore m =
      if S.member m mySites
        then 2
        else 30

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
    (S.map river (S.filter (\c -> Types.punter c == p) (claims gs)))

score :: Map -> Set River -> Int
score mp rs =
  sum [shortestDist m s ^ 2 | m <- S.toList (mines mp), s <- reachableSites m]
  where
    fullGraph = graph mp
    claimGraph = graphOfRivers rs
    reachableSites m = BFS.bfs m claimGraph
    shortestDist m s = length $ tail $ BFS.esp m s fullGraph
