module Main where

import System.Environment (getArgs)
import WordleSolver

-- Main function that starts the interaction with user
main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then putStrLn "Usage: stack run <wordlist-file> <secret-word-file>"
        else do
            let wordListFile = args !! 0
            let secretWordFile = args !! 1
            wordList <- loadWords wordListFile
            secretWord <- loadSecretWord secretWordFile
            putStrLn "Begin"
            let firstGuess = bestFirstGuess wordList
            putStrLn $ "Best first guess: " ++ firstGuess
            gameLoop wordList secretWord

-- Get input until the victory and update the possible words list
gameLoop :: [String] -> String -> IO ()
gameLoop wordList secretWord = do
    guess <- getValidInput "Enter word: "
    let clues = getClues secretWord guess
    putStrLn $ "Clues: " ++ clues
    if guess == secretWord
        then putStrLn "Victory"
        else do
            let updatedWordList = filterWords (filter (\w -> length w == 5) wordList) clues guess
            putStrLn $ "Dictionary: " ++ show updatedWordList
            gameLoop updatedWordList secretWord
