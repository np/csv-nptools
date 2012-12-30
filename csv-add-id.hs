import System.Environment
import Data.Char
import Utils

addIdLn :: Int -> [String] -> [String]
addIdLn 0 = ("ID":)
addIdLn i = (show i:)

addId :: Int -> FilePath -> IO ()
addId i = interactTable $ zipWith addIdLn [i..]

main :: IO ()
main = do
    args <- getArgs
    let (args2, start) =
          case args of
            "--help":_ -> err
            "--start":i:xs
              | all isDigit i -> (xs, read i)
            _                 -> (args, 0)
    addId start $ getInput err args2
  where err = error "Usage: csv-add-id [--start <int>] [<file>|-]"
