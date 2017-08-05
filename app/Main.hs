{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Semigroup ((<>))
import Offline
import System.IO (stderr)

main :: IO ()
main = play "flux-ambassadors" reader writer
  where
    reader = do
      l <- B.getLine
      B.hPutStrLn stderr ("Received: " <> l)
      return (BSL.fromStrict l)
    writer :: BSL.ByteString -> IO ()
    writer s =
      let s' = (BSL.toStrict s)
      in do B.hPutStrLn stderr ("Sending:  " <> s')
            B.putStr s'
