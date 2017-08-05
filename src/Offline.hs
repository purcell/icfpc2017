{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Offline where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import Types

data Hello = Hello
  { me :: String
  } deriving (Generic, ToJSON)

data HelloBack = HelloBack
  { you :: String
  } deriving (Generic, FromJSON)

data Ready =
  Ready PunterID
        GameState

instance ToJSON Ready where
  toJSON (Ready p s) = object ["punter" .= p, "state" .= s]

data GameState = GameState
  { initialState :: SetupState
  , prevMoves :: [Move]
  } deriving (Generic, FromJSON, ToJSON)

data Action
  = Setup SetupState
  | Play [Move]
         GameState
  | Stop [Move]
         [Score]
         GameState

data Score =
  Score PunterID
        Int
  deriving (Show)

instance FromJSON Score where
  parseJSON =
    withObject "score" (\o -> Score <$> o .: "punter" <*> o .: "score")

instance FromJSON Action where
  parseJSON =
    withObject
      "action"
      (\o ->
         (Setup <$> parseJSON (Object o)) <|>
         (Play <$> (o .: "move" >>= parseMoves) <*> o .: "state") <|>
         (Stop <$> (o .: "moves") <*> (o .: "scores") <*> o .: "state"))
    where
      parseMoves = withObject "moves" (.: "moves")

data SetupState = SetupState
  { punter :: PunterID
  , punters :: Int
  , map :: Map
  } deriving (Generic, FromJSON, ToJSON)

precomputeGameState :: SetupState -> GameState
precomputeGameState s = GameState s []

updateState :: [Move] -> GameState -> GameState
updateState moves s = s {prevMoves = (prevMoves s <> moves)}

nextMove :: GameState -> (Move, GameState)
nextMove s = (Pass (myPunterID s), s)

myPunterID :: GameState -> PunterID
myPunterID s = Offline.punter (initialState s)

data Turn =
  Turn Move
       GameState

instance ToJSON Turn where
  toJSON (Turn m s) =
    case toJSON m of
      (Object o) -> Object (HM.insert "state" (toJSON s) o)
      _ -> error "kaboom"

play :: String -> IO BL.ByteString -> (BL.ByteString -> IO ()) -> IO ()
play myname reader writer = do
  handshake
  action <- recv
  case action of
    Setup state
          -- TODO: print out setup time?
     ->
      let state' = precomputeGameState state
      in send (Ready (myPunterID state') state')
    Play moves state ->
      let (move, state') = nextMove (updateState moves state)
      in send $ Turn move state'
    Stop moves scores state -> do
      hPutStrLn stderr (show scores)
      let state' = updateState moves state
      return ()
  where
    handshake = do
      send (Hello myname)
      (HelloBack name) <- recv
      guard $ name == myname
    send :: ToJSON a => a -> IO ()
    send o = writer (encode o)
    recv :: FromJSON a => IO a
    recv = do
      input <- reader
      case eitherDecode input of
        Right o -> pure o
        Left e -> fail e
