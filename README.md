# Wordle Solver 

## The Task

- Console (or other user-) interface
- The user choses a word and you return the clues which letters of the user's input is in the word and which are at their correct positions
- A search for words that satisfy the known clues
- Error reporting
- Unit tests
- Compute the best first word, meaning the word that ensures that you can guess any word from a dictionary in the minimum number of steps. Here the dictionary is a parameter.

## The Architecture
The architecture of your solution.
Why certain libraries were chosen.


## The Performance
I didn't used any string libraries that improve performance yet, so without the BFG (Best First Guess) it flies even on a big dictionaries (2300+ words). With the BFG it takes about 0.2s on a 100-words dict, about 5 seconds on a 400-words dict. And it dies on a dict with 2300 words. 
