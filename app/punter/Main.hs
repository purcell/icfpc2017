{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Semigroup ((<>))
import Offline
import Strategy
import System.IO (hFlush, hPutStrLn, stderr, stdin, stdout)

main :: IO ()
main = play nextMoveMST "flux-ambassadors" reader writer
  where
    reader = do
      firstChunk <- B.hGet stdin 10
      let (initial, rest) = B.break (== ':') firstChunk
          len = read (B.unpack initial)
          afterColon = B.tail rest
      do more <- B.hGet stdin (len - B.length afterColon)
         let msg = afterColon <> more
         B.hPutStrLn stderr ("Received: " <> firstChunk <> more)
         return (BSL.fromStrict msg)
    writer :: BSL.ByteString -> IO ()
    writer s =
      let msg = show (BSL.length s) <> ":" <> BSL.unpack s
      in do hPutStrLn stderr ("Sending:  " <> msg)
            putStr msg
            hFlush stdout
