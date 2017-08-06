{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GamePlay where

import Data.Aeson
import Data.Foldable (concatMap, sum)
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

moveAndUpdate :: GameState -> GameState
moveAndUpdate state = updateState [move] state'
  where
    (move, state') = nextMove state

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
