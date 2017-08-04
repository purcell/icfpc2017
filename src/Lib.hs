module Lib
  ( readMap
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Types

readMap :: FilePath -> IO (Either String Map)
readMap file = do
  contents <- BL.readFile file
  return $ eitherDecode contents
