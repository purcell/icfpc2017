{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Set (Set)
import GHC.Generics (Generic)

type SiteID = Int

type PunterID = Int

data Site = Site
  { id :: SiteID
  , x :: Double
  , y :: Double
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data River = River
  { source :: SiteID
  , target :: SiteID
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON) -- TODO: custom Eq, Ord

data Map = Map
  { sites :: Set Site
  , rivers :: Set River
  , mines :: Set SiteID
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Claim = Claim
  { punter :: PunterID
  , river :: River
  } deriving (Eq, Ord, Show)

data Move
  = ClaimMove Claim
  | PassMove Pass
  deriving (Show)

data Pass =
  Pass PunterID
  deriving (Show)

instance ToJSON Pass where
  toJSON (Pass p) = object ["punter" .= p]

instance FromJSON Pass where
  parseJSON (Object o) = Pass <$> o .: "punter"
  parseJSON invalid = typeMismatch "Pass" invalid

instance ToJSON Claim where
  toJSON (Claim p (River s t)) =
    object ["punter" .= p, "source" .= s, "target" .= t]

instance FromJSON Claim where
  parseJSON (Object o) = do
    s <- o .: "source"
    t <- o .: "target"
    Claim <$> o .: "punter" <*> pure (River s t)
  parseJSON invalid = typeMismatch "Claim" invalid

instance ToJSON Move where
  toJSON (ClaimMove c) = object ["claim" .= toJSON c]
  toJSON (PassMove p) = object ["pass" .= toJSON p]

instance FromJSON Move where
  parseJSON (Object o) =
    (ClaimMove <$> (o .: "claim")) <|> (PassMove <$> o .: "pass")
  parseJSON invalid = typeMismatch "NewOccurrence" invalid
