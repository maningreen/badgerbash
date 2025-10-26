module Main (main) where

data WordItem = WordItem {
    _word :: String,
    _weight :: Float
  }
type WordBank = [WordItem]

wordBankPath :: FilePath
wordBankPath = "wordbank"

main :: IO ()
main = return ()
