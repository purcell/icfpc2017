module Strategy where

import Data.List as List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Set (Set, (\\))
import GamePlay
import Types

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
