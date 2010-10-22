import System.Environment
import Database.TxtSushi.FlatFile

type Tbl = [[String]]

mapTable ::(Tbl -> Tbl) -> String -> String
mapTable f = formatTable csvFormat . f . parseTable csvFormat

getContentsFromFileOrStdin :: FilePath -> IO String
getContentsFromFileOrStdin "-"  = getContents
getContentsFromFileOrStdin fp   = readFile fp

interactTable ::(Tbl -> Tbl) -> FilePath -> IO ()
interactTable f = (putStr . mapTable f =<<) . getContentsFromFileOrStdin

addIdLn :: Int -> [String] -> [String]
addIdLn 0 row = "ID":row
addIdLn i row = show i:row

addId :: FilePath -> IO ()
addId = interactTable (zipWith addIdLn [0..])

main :: IO ()
main = do
    args <- getArgs
    case args of
      [fileArg]  -> addId fileArg
      []         -> addId "-"
      _          -> error "Usage: csv-add-id [<file-or-dash>]"
