module Strategy where

import Data.List as List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Set (Set, (\\))
import GamePlay
import Types

data Weights = Weights
  { wConnectedMineAgain :: Int
  , wConnectedNewMine :: Int
  , wEndTouchingMyRiver :: Int
  } deriving (Show)

defaultWeights :: Weights
defaultWeights = Weights 2 30 30

nextMove :: Weights -> GameState -> (Move, GameState)
nextMove ws s = (ClaimMove bestClaim, s)
  where
    bestClaim = Claim (myPunterID s) (bestUnclaimedRiver ws s)

bestUnclaimedRiver :: Weights -> GameState -> River
bestUnclaimedRiver w s =
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
           if myPunterID s == Types.punter c
             then [source (river c), target (river c)]
             else [])
        (S.toList (claims s))
    mineSites :: Set SiteID
    mineSites = mines theMap
    riverScore :: River -> Int
    riverScore r =
      (1 + sum (connectedMineScore <$> connectedMineSites r)) *
      (wEndTouchingMyRiver w ^ endsTouchingMyRivers r)
    endsTouchingMyRivers :: River -> Int
    endsTouchingMyRivers r = S.size (S.filter (connectedTo r) mySites)
    connectedMineSites :: River -> [SiteID]
    connectedMineSites r = S.toList $ S.filter (connectedTo r) mineSites
    connectedMineScore :: SiteID -> Int
    connectedMineScore m =
      if S.member m mySites
        then wConnectedMineAgain w
        else wConnectedNewMine w
