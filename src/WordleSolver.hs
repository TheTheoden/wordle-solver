module WordleSolver (
    getClues,
    filterWords,
    validateInput,
    loadWords,
    loadSecretWord,
    prompt,
    getValidInput,
    bestFirstGuess
) where

import System.IO (hFlush, stdout)
import Data.Char (isLower)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Print a prompt and get user input
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- Generate clues
getClues :: String -> String -> String
getClues secret guess = map matchChar (zip secret guess)
  where
    matchChar (s, g)
      | s == g    = 'G'        -- Correct letter, correct position
      | g `elem` secret = 'Y'  -- Correct letter, wrong position
      | otherwise = 'X'        -- Wrong letter

-- Filter words based on clues
filterWords :: [String] -> String -> String -> [String]
filterWords wordList clues guess = filter matchesClues wordList
  where
    matchesClues word = all (match word) (zip3 word guess clues)
    match word (w, g, c)
      | c == 'G' = w == g
      | c == 'Y' = w /= g && g `elem` word
      | c == 'X' = g `notElem` word
      | otherwise = False

-- Validate input
validateInput :: String -> Either String String
validateInput input
  | length input /= 5 = Left "Input must be 5 letters"
  | any (not . isLower) input = Left "Input must be english lowercase"
  | otherwise = Right input

-- Load dictionary from a file
loadWords :: FilePath -> IO [String]
loadWords filePath = do
    contents <- readFile filePath
    return $ lines contents

-- Load the secret word from a file
loadSecretWord :: FilePath -> IO String
loadSecretWord filePath = do
    contents <- readFile filePath
    let word = head (lines contents)
    return word

-- Get input and throw errors if any
getValidInput :: String -> IO String
getValidInput promptText = do
    input <- prompt promptText
    case validateInput input of
      Left err -> do
        putStrLn $ "Error: " ++ err
        getValidInput promptText
      Right validInput -> return validInput

-- Find the best first guess
bestFirstGuess :: [String] -> String
bestFirstGuess wordList =
    fst $ maximumBy (comparing snd) $ map (\w -> (w, maxEliminationsForFirstGuess w)) wordList
  where
    maxEliminationsForFirstGuess secretWord =
        maximum $ map (\firstGuess -> eliminationsForGuess secretWord firstGuess) wordList
    eliminationsForGuess secretWord firstGuess =
        length $ filterWords wordList (getClues secretWord firstGuess) firstGuess
