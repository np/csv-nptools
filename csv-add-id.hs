import System.Environment
import Database.TxtSushi.FlatFile
import Data.Char
import Utils

addIdLn :: Int -> [String] -> [String]
addIdLn 0 row = "ID":row
addIdLn i row = show i:row

addId :: Int -> FilePath -> IO ()
addId i = interactTable (zipWith addIdLn [i..])

main :: IO ()
main = do
    args <- getArgs
    let (args2, start) =
          case args of
            "--start":i:xs
              | all isDigit i -> (xs, read i)
            _                 -> (args, 0)
    case args2 of
      [fileArg]  -> addId start fileArg
      []         -> addId start "-"
      _          -> error "Usage: csv-add-id [--start <int>] [<file>|-]"
