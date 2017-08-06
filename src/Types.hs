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
  , loc :: Maybe (Double, Double)
  } deriving (Eq, Ord, Show)

instance FromJSON Site where
  parseJSON =
    withObject
      "site"
      (\o -> do
         x <- o .:? "x"
         y <- o .:? "y"
         i <- o .: "id"
         return $ Site i ((,) <$> x <*> y))

instance ToJSON Site where
  toJSON (Site i l) =
    object $
    ["id" .= i] ++
    (case l of
       Just (x, y) -> ["x" .= x, "y" .= y]
       _ -> [])

data River = River
  { source :: SiteID
  , target :: SiteID
  } deriving (Show, Generic, FromJSON, ToJSON)

instance Eq River where
  (==) (River s t) (River s' t') = (s, t) == (s', t') || (s, t) == (t', s')

instance Ord River where
  (<=) r1 r2 = minRiver r1 <= minRiver r2
    where
      minRiver (River s t) = minimum [(s, t), (t, s)]

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
  | Pass PunterID
  | Splurge PunterID
            [SiteID]
  deriving (Show)

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
  toJSON (Pass p) = object ["pass" .= object ["punter" .= p]]
  toJSON (Splurge p route) =
    object ["splurge" .= object ["punter" .= p, "route" .= route]]

instance FromJSON Move where
  parseJSON (Object o) =
    (ClaimMove <$> (o .: "claim")) <|>
    (o .: "pass" >>= (\o2 -> Pass <$> o2 .: "punter")) <|>
    (o .: "splurge" >>= (\o2 -> Splurge <$> o2 .: "punter" <*> o2 .: "route"))
  parseJSON invalid = typeMismatch "NewOccurrence" invalid
