import System.Environment (getArgs)
import Utils (interactTable,padTbl,getInput)

main :: IO ()
main = interactTable (padTbl "") . getInput err =<< getArgs
  where err = error "Usage: csv-pad [<file>|-]"
