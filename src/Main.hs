module Main (main) where
import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle.FD (openFile)
import GHC.IO.IOMode (IOMode(ReadMode))

type WordBank = [String]

wordBankPath :: FilePath
wordBankPath = "wordbank"

loadWordBank :: IO WordBank
loadWordBank = openFile wordBankPath ReadMode >>= fmap lines . hGetContents

main :: IO ()
main = loadWordBank >>= print 
