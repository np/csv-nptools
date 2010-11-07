import Data.List (transpose)
import System.Environment (getArgs)
import Utils (Tbl,interactTable,padTbl)

transposeCSV :: Tbl -> Tbl
transposeCSV = transpose . padTbl ""

main :: IO ()
main = do args <- getArgs
          case args of
            []  -> interactTable transposeCSV "-"
            [f] -> interactTable transposeCSV f
            _   -> error "Usage: csv-transpose [<file>|-]"

