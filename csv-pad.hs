import System.Environment (getArgs)
import Utils (interactTable,padTbl)

main :: IO ()
main = do args <- getArgs
          let f = case args of
                    []  -> "-"
                    [s] -> s
                    _   -> error "Usage: csv-pad [<file>|-]"
          interactTable (padTbl "") f
