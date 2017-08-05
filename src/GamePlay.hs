{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GamePlay where

import Data.Aeson
import Data.Foldable (concatMap)
import Data.List (sortBy)
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
claims =
  S.fromList .
  concatMap
    (\m ->
       case m of
         ClaimMove c -> pure c
         Pass _ -> mempty) .
  prevMoves

precomputeGameState :: SetupState -> GameState
precomputeGameState s = GameState s []

updateState :: [Move] -> GameState -> GameState
updateState moves s = s {prevMoves = (prevMoves s <> moves)}

myPunterID :: GameState -> PunterID
myPunterID s = GamePlay.punter (initialState s)

nextMove :: GameState -> (Move, GameState)
nextMove s = (ClaimMove bestClaim, s)
  where
    bestClaim = Claim (myPunterID s) (bestUnclaimedRiver s)

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
      (1 + 30 * S.size (S.filter (connectedTo r) mineSites)) *
      (1 + 10 * S.size (S.filter (connectedTo r) mySites))