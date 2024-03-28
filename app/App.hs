module App
    ( AppState
    , initialState
    , updateState
    , renderState
    ) where

import qualified Data.Char as Char

data AppState = AppState
  { guesses :: [String]
  , currentGuess :: String
  }

initialState :: AppState
initialState = AppState { guesses = [], currentGuess = "" }

data Input
  = Letter Char
  | Backspace
  | Enter

parseInput :: [Int] -> Maybe Input
parseInput [0x7f] = Just Backspace
parseInput [0xa] = Just Enter
parseInput [keyCode] | Char.isLetter letter = Just $ Letter letter
  where letter = toEnum keyCode
parseInput _ = Nothing

updateState :: [Int] -> AppState -> AppState
updateState keyCodes appState =
  case parseInput keyCodes of
    Nothing                                                  -> appState
    Just (Letter letter) | length appState.currentGuess < 5  -> appState { currentGuess = appState.currentGuess <> [letter] }
                         | otherwise                         -> appState
    Just Backspace                                           -> appState { currentGuess = take (length appState.currentGuess - 1) appState.currentGuess }
    Just Enter           | length appState.currentGuess == 5 -> appState { currentGuess = "", guesses = appState.guesses <> [appState.currentGuess] }
                         | otherwise                         -> appState

renderState :: AppState -> String
renderState appState =
  guesses <> toSquareLetters appState.currentGuess
    where
      guesses = concatMap (\guess -> toSquareLetters guess <> "\n") appState.guesses
      toSquareLetters = concatMap toLetter
      toLetter char = [toEnum (Char.ord char - Char.ord 'a' + Char.ord '🅰'), ' ']
