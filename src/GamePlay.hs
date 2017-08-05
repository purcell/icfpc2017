{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GamePlay where

import Data.Aeson
import Data.Semigroup ((<>))
import Data.Set (Set)
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

nextMove :: GameState -> (Move, GameState)
nextMove s = (Pass (myPunterID s), s)

myPunterID :: GameState -> PunterID
myPunterID s = GamePlay.punter (initialState s)
