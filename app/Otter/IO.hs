module Otter.IO
    ( getKey
    ) where

import System.IO qualified as IO

getKey :: IO String
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- IO.hReady IO.stdin
      (if more then getKey' else return) (char : chars)
