import Data.List (transpose)
import System.Environment (getArgs)
import Utils (interactTable,padTbl,getInput)

main :: IO ()
main = interactTable (transpose . padTbl "") . getInput err =<< getArgs
  where err = error "Usage: csv-transpose [<file>|-]"

