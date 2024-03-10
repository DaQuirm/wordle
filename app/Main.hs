{-#LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Char (isLower, ord)
import Data.List (nub, (!?), sortOn, nubBy, find, (\\), intersperse)
import System.Random (getStdRandom, Random(randomR))
import System.Console.ANSI (setSGR, SGR (..), ConsoleLayer (..), ColorIntensity (..), Color (..))


letterMatches :: String -> (Int, Char) -> [(Int, Int, Char)]
letterMatches word (letterIndex, letter) =
  map
    (\(i, l) -> (i, letterIndex, l))
    (filter (\(_, s) -> s == letter) (zip [0..] word))

wordMatches :: String -> String -> [(Int, Int, Char)]
wordMatches secret guess =
  concatMap (letterMatches secret) (zip [0..] guess)

pickMatches :: [(Int, Int, Char)] -> [(Int, Int, Char)]
pickMatches matches =
  nubBy
    (\(si, gi, _) (si', gi', _) -> si == si' || gi == gi')
    (sortOn (\(a, b, _) -> a /= b) matches)

renderMatches :: String -> [(Int, Int, Char)] -> IO ()
renderMatches wordleWord matches = do
  mapM_
    (\(letterIndex, letter) -> do
      let (intensity, color) =
            case find (\(_, guessLetterIndex, _) -> guessLetterIndex == letterIndex) matches of
              Nothing                    -> (Dull, White)
              Just (a, b, _) | a == b    -> (Vivid, Green)
                             | otherwise -> (Vivid, Yellow)
      setSGR [SetColor Foreground intensity color]
      putStr [toEnum (ord letter - ord 'a' + ord '🅰'), ' ']
    )
    (zip [0..] wordleWord)
  setSGR [SetColor Foreground Dull White]
  putStrLn "" -- for cross-platform newline

data Env =
  Env
    { wordleWords :: [String]
    , wordleWord :: String
    }

guessingLogic :: Env -> Int -> [String] -> IO ()
guessingLogic env attemptsLeft attempts =
  if attemptsLeft == 0 then
    putStrLn $ "You have no attempts left. The word was " ++ show env.wordleWord ++ ". Try again. "
  else do
    putStrLn "Guess a 5-letter word"
    guess <- getLine
    if guess `elem` env.wordleWords
      then do
        let matches = (pickMatches $ wordMatches env.wordleWord guess)
        renderMatches guess matches
        if all (\(a, b, _) -> a == b) matches && length matches == 5
          then
            putStrLn "You guessed correctly!"
          else do
            let attempts' = guess : attempts
            putStrLn $ intersperse ' ' $ ['a'..'z'] \\ (concat attempts')
            guessingLogic env (attemptsLeft - 1) attempts'
      else do
        putStrLn $ guess <> " is not a word!"
        guessingLogic env attemptsLeft attempts

main :: IO ()
main = do
  alice <- readFile "words.txt"
  let wordleWords = nub $ filter (\s -> length s == 5 && all isLower s) (words alice)
  wordIndex <- getStdRandom $ randomR (0, length wordleWords)

  case wordleWords !? wordIndex of
    Nothing -> putStrLn "The impossible happened!"
    Just wordleWord -> guessingLogic (Env { wordleWords, wordleWord }) 5 []
