module Main (main) where
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import GHC.IO.Handle (hGetContents)

data WordItem = WordItem {
    _word :: String,
    _weight :: Float
  } deriving (Show)
type WordBank = [WordItem]

wordBankPath :: FilePath
wordBankPath = "wordbank"

-- {-# WARNING Impartial function #-}
parseWord :: String -> WordItem
parseWord x = WordItem {
    _word = word,
    _weight = read weightStr 
  }
  where
    [word, weightStr] = words x

parseWordBank :: String -> WordBank
parseWordBank = map parseWord . lines

readWordBank :: FilePath -> IO WordBank
readWordBank x = fmap (parseWordBank) . hGetContents =<< openFile x ReadMode  

main :: IO ()
main = do
  readWordBank wordBankPath >>= print
  return ()
