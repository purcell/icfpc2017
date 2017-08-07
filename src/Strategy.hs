{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Strategy where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MST as MST
import Data.List as List (sortBy)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Set (Set, (\\))
import GamePlay
import Types

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

data ClaimState
  = AlreadyClaimed
  | Claimable
  | Optionable
  | Unavailable
  deriving (Eq)

nextMoveMST :: Weights -> GameState -> (Move, GameState)
nextMoveMST _ s = (bestMove, s)
  where
    bestMove =
      case claimState bestRiver of
        Claimable -> ClaimMove me bestRiver
        Optionable -> Option me bestRiver
        _ -> error "unexpected claim state for best river"
    mp = GamePlay.map (initialState s)
    me = myPunterID s
    g = graphOfRivers (S.filter navigable (rivers mp))
    optionsLeft = optionsRemaining s me
    allMines = mines mp
    weightedGraph = G.emap edgeWeight g
    navigable r =
      case claimState r of
        AlreadyClaimed -> True
        Claimable -> True
        _ -> False
    edgeWeight r =
      case claimState r of
        AlreadyClaimed -> 0
        Optionable -> 50
        Claimable -> 10
        _ -> error "unavailable river in graph"
    pathsFromMines :: [[SiteID]]
    pathsFromMines =
      (fmap fst . G.unLPath) <$>
      concatMap (`MST.msTreeAt` weightedGraph) (S.toList allMines)
    bestRiver =
      case listToMaybe $ filter claimable bestRivers of
        Just r -> r
        _ -> bestUnclaimedRiver defaultWeights s
          -- error "Didn't find a best river"
    claimState r =
      case M.lookup r riverClaims of
        Just ps
          | me `elem` ps -> AlreadyClaimed
        Just [] -> Claimable
        Just [_]
          | optionsLeft > 0 -> Optionable
        Just _ -> Unavailable
        Nothing -> error "missing river"
    riverClaims = claimants mp (prevMoves s)
    claimable :: River -> Bool
    claimable r =
      case claimState r of
        Claimable -> True
        Optionable -> True
        _ -> False
    -- Most critical rivers first
    bestRivers :: [River]
    bestRivers
      -- fmap fst $
      -- sortBy (flip (comparing snd)) $
      -- frequency $
     = concatMap (S.toList . pathToRivers) pathsFromMines

data Weights = Weights
  { wConnectedNewMine :: Int
  , wEndTouchingMyRiver :: Int
  , wOnShortestMinePath :: Int
  } deriving (Show)

defaultWeights :: Weights
defaultWeights = Weights 30 30 10

data RiverFactors = RiverFactors
  { rfEndsTouchingMyRivers :: Int
  , rfShortestMinePathCount :: Int
  , rfConnectedNewMines :: Int
  , rfNumPathsBetweenMines :: Int
  , rfOptionsRemaining :: Int
  }

riverScore :: Weights -> RiverFactors -> Int
riverScore w f =
  product
    [ 1 + wConnectedNewMine w * rfConnectedNewMines f
    , wEndTouchingMyRiver w ^ rfEndsTouchingMyRivers f
    , wOnShortestMinePath w ^
      (2 * rfShortestMinePathCount f `div` rfNumPathsBetweenMines f)
    ]

nextMove :: Weights -> GameState -> (Move, GameState)
nextMove ws s = (ClaimMove (myPunterID s) (bestUnclaimedRiver ws s), s)

bestUnclaimedRiver :: Weights -> GameState -> River
bestUnclaimedRiver w s =
  case listToMaybe $
       sortBy (flip $ comparing (riverScore w . riverFactors)) unclaimedRivers of
    Just r -> r
    _ -> error "how could there not be an unclaimed river?"
  where
    theMap = GamePlay.map (initialState s)
    -- TODO: include rivers where options are available
    unclaimedRivers = S.toList $ allRivers \\ claimedRivers
    allRivers = rivers theMap
    claimedRivers = S.map claimRiver (claims s)
    mySites :: Set SiteID
    mySites =
      S.fromList $
      concatMap
        (\c ->
           if myPunterID s == claimPunter c
             then [source (claimRiver c), target (claimRiver c)]
             else [])
        (S.toList (claims s))
    mineSites :: Set SiteID
    mineSites = mines theMap
    riverFactors r =
      RiverFactors
      { rfEndsTouchingMyRivers = S.size (S.filter (connectedTo r) mySites)
      , rfShortestMinePathCount =
          sum [1 | p <- shortestMinePaths s, r `S.member` p]
      , rfConnectedNewMines =
          sum [1 | m <- connectedMineSites r, m `S.notMember` mySites]
      , rfNumPathsBetweenMines = length $ shortestMinePaths s
      , rfOptionsRemaining = optionsRemaining s (myPunterID s)
      }
    connectedMineSites :: River -> [SiteID]
    connectedMineSites r = S.toList $ S.filter (connectedTo r) mineSites
