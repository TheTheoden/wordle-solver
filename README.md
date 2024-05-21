# Wordle Solver - The Report

## The Task

- Console (or other user-) interface
- The user choses a word and you return the clues which letters of the user's input is in the word and which are at their correct positions
- A search for words that satisfy the known clues
- Error reporting
- Unit tests
- Compute the best first word, meaning the word that ensures that you can guess any word from a dictionary in the minimum number of steps. Here the dictionary is a parameter.

## How to launch

I made my project with `stack new wordle-solver` + `stack setup` 

So in order to run it you can use the `stack build` + `stack run <dict-location> <secret-word-location>`

Where `<dict-location>` is the path to the dict of words separated by new lines and `<secret-word-location>` is a path to the file with a single line of a secret word.

## The Architecture

### Main.hs

Contains the basic console interactions with user and the main loop of the game

### WordleSolver.hs

All the logic is defined here. The interesting functions are:

- `getClues` - evaluates the guess assigning the "G", "Y" or "X" to each letter depending on if it was a correct letter in a correct position, a correct letter in incorrect position or a incorrect letter accordingly.
- `filterWords` - filters out the words that can't satisfy the existing clues
- `bestFirstGuess` - having a dictionary of words as an input returns the best first word. The 'best' is considered by testing every first word against all over words as a secret ones and comparing, what amount of words would be eliminated. Performs in O(n^3) operations on the strings of length 5 which is quite slow.

### Spec.hs

The small amount of unit tests is located here. I used hspec.

## The Libraries

- `System` for basic interactions with IO and user
- `Data.Char` for `isLower` to make sure that user uses lowercase in order to simplify checking
- `Data.List` for `maximumBy` to find the BFG by comparing all the words in dictionary as the starting ones
- `Data.Ord` for `comparing` to compare tuples by their second value, number of eliminated words in our case.
- `Test.Hspec` for unit-testing

## The Performance
I didn't used any string libraries that improve performance yet, so without the BFG (Best First Guess) it flies (<0.5s) even on a big dictionaries (2300+ words). With the BFG it takes about 0.2s on a 100-words dict, about 5 seconds on a 400-words dict. And it dies (>60s) on a dict with 2300 words. 
I believe that the usage of unboxed strings would have increased the performance dramatically, but I'm not ready for it yet. 
