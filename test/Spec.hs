module Main where

import Test.Hspec
import WordleSolver (getClues, filterWords)

main :: IO ()
main = hspec $ do
  describe "getClues" $ do
    it "returns correct clues" $ do
      getClues "apple" "apple" `shouldBe` "GGGGG"
      getClues "apple" "apply" `shouldBe` "GGGGX"
      getClues "apple" "plumb" `shouldBe` "YYXXX"
      getClues "apple" "crane" `shouldBe` "XXYXG"
      getClues "apple" "other" `shouldBe` "XXXYX"

  describe "filterWords" $ do
    it "filters words correctly" $ do
      let wordList = ["apple", "apply", "plumb", "crane", "tripe"]
      filterWords wordList "GGGGX" "apply" `shouldBe` ["apple"]
      filterWords wordList "YYXXX" "plumb" `shouldBe` ["apple", "apply"]
      filterWords wordList "XXYXG" "crane" `shouldBe` ["apple"]
      filterWords wordList "XXXYX" "other" `shouldBe` ["apple"]
