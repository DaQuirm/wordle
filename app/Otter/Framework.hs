module Otter.Framework
    ( OtterEnv(..)
    , run
    ) where

import System.IO qualified as IO
import System.Console.ANSI qualified as Console
import Data.IORef qualified as IORef
import Control.Monad (forever)
import Otter.IO (getKey)
import Data.Char (ord)

data OtterEnv state = OtterEnv
  { initialState :: state
  , update :: [Int] -> state -> state
  , render :: state -> String
  }

run :: OtterEnv state -> IO ()
run env = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False

  stateRef <- IORef.newIORef env.initialState

  forever $ do
    Console.setCursorPosition 0 0
    Console.clearScreen

    appState <- IORef.readIORef stateRef
    putStrLn $ env.render appState

    input <- getKey

    let keyCodes = map ord input

    IORef.writeIORef stateRef $ env.update keyCodes appState
