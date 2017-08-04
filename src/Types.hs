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
  } deriving (Eq, Ord, Show, Generic)

data River = River
  { source :: SiteID
  , target :: SiteID
  } deriving (Eq, Ord, Show, Generic) -- TODO: custom Eq, Ord

data Map = Map
  { sites :: Set Site
  , rivers :: Set River
  , mines :: Set SiteID
  } deriving (Eq, Ord, Show, Generic)

data Move
  = Claim { punter :: PunterID
          , river :: River }
  | Pass { punter :: PunterID }
  deriving (Eq, Show)
