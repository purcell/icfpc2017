{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Strategy where

import Data.Foldable (asum)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MST as MST
import Data.List as List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust, listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Set (Set, (\\))
import GamePlay
import Types

nextMoveFirstUnclaimed :: GameState -> (Move, GameState)
nextMoveFirstUnclaimed s =
  (ClaimMove (myPunterID s) (head (S.toList available)), s)
  where
    riverClaims = claimStates s
    available = filterRiversByClaim riverClaims (== Unclaimed)

frequency :: (Ord a) => [a] -> M.Map a Int
frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

nextMoveMST :: GameState -> (Move, GameState)
nextMoveMST s = (bestMove, s)
  where
    bestMove =
      case claimState bestRiver of
        Unclaimed -> ClaimMove me bestRiver
        Optionable -> Option me bestRiver
        _ -> error "unexpected claim state for best river"
    mp = GamePlay.map (initialState s)
    me = myPunterID s
    riversFromMines = mstClaimableRiversFromMines (mines mp) riverClaims
    closestRiversToMines = snd <$> sortBy (comparing fst) riversFromMines
    bestRiver =
      fromJust $
      asum
        [listToMaybe closestRiversToMines, (listToMaybe (S.toList unclaimed))]
    riverClaims = claimStates s
    claimState r = riverClaims M.! r
    unclaimed = filterRiversByClaim riverClaims (== Unclaimed)

nextMoveMSTSmart :: GameState -> (Move, GameState)
nextMoveMSTSmart s = (bestMove, s)
  where
    bestMove =
      case claimState bestRiver of
        Unclaimed -> ClaimMove me bestRiver
        Optionable -> Option me bestRiver
        _ -> error "unexpected claim state for best river"
    mp = GamePlay.map (initialState s)
    me = myPunterID s
    claimableRiversFromMines =
      mstClaimableRiversFromMines (mines mp) riverClaims
    riversNextToMines = snd <$> filter ((== 0) . fst) claimableRiversFromMines
    riversOnHotPaths =
      fmap fst $
      takeWhile ((> 1) . snd) $
      sortBy (flip (comparing snd)) $
      M.toList (frequency (snd <$> claimableRiversFromMines))
    bestRiver =
      fromJust $
      asum
        [ listToMaybe riversOnHotPaths
        , listToMaybe riversNextToMines
        , listToMaybe (snd <$> claimableRiversFromMines)
        , listToMaybe (S.toList unclaimed)
        ]
    riverClaims = claimStates s
    claimState r = riverClaims M.! r
    unclaimed = filterRiversByClaim riverClaims (== Unclaimed)

mstClaimableRiversFromMines ::
     Set SiteID -> M.Map River ClaimState -> [(Int, River)]
mstClaimableRiversFromMines allMines riverClaims =
  filter ((`S.member` claimableRivers) . snd) $
  concatMap (zip [0 ..] . pathToRivers) pathsFromMines
  where
    claimableRivers = filterRiversByClaim riverClaims claimable
    g = graphOfRivers (filterRiversByClaim riverClaims potentiallyNavigable)
    weightedGraph = G.emap edgeWeight g
    edgeWeight r =
      case claimState r of
        AlreadyClaimed -> 0
        Optionable -> 50
        Unclaimed -> 10
        _ -> error "unavailable river in graph"
    pathsFromMines :: [[SiteID]]
    pathsFromMines =
      (fmap fst . G.unLPath) <$>
      concatMap (`MST.msTreeAt` weightedGraph) (S.toList allMines)
    claimState r = riverClaims M.! r

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
  } deriving (Eq, Ord)

defaultFactors :: RiverFactors
defaultFactors = RiverFactors 0 0 0 0 0

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
bestUnclaimedRiver w s = bestRankedRiver w (basicRiverFactors s unclaimedRivers)
  where
    theMap = GamePlay.map (initialState s)
    -- TODO: include rivers where options are available
    unclaimedRivers = allRivers \\ claimedRivers
    allRivers = rivers theMap
    claimedRivers = S.map claimRiver (claims s)

bestRankedRiver :: Weights -> Set (River, RiverFactors) -> River
bestRankedRiver w riversAndFactors =
  case listToMaybe rankedRivers of
    Just (r, _) -> r
    _ -> error "no river to rank"
  where
    rankedRivers =
      sortBy (flip $ comparing (riverScore w . snd)) $ S.toList riversAndFactors

basicRiverFactors :: GameState -> Set River -> Set (River, RiverFactors)
basicRiverFactors s = S.map (\r -> (r, riverFactors r))
  where
    theMap = GamePlay.map (initialState s)
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
      defaultFactors
      { rfEndsTouchingMyRivers = S.size (S.filter (connectedTo r) mySites)
      , rfShortestMinePathCount =
          sum [1 | p <- shortestMinePaths s, r `S.member` p]
      , rfConnectedNewMines =
          sum [1 | m <- connectedMineSites r, m `S.notMember` mySites]
      , rfNumPathsBetweenMines = length $ shortestMinePaths s
      , rfOptionsRemaining =
          optionsRemaining theMap (prevMoves s) (myPunterID s)
      }
    connectedMineSites :: River -> [SiteID]
    connectedMineSites r = S.toList $ S.filter (connectedTo r) mineSites
