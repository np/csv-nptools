import System.Environment
import System.IO

import Database.TxtSushi.FlatFile
import Database.TxtSushi.IOUtil

import Data.Char

type Tbl = [[String]]

mapTable ::(Tbl -> Tbl) -> String -> String
mapTable f = formatTable csvFormat . f . parseTable csvFormat

interactTable ::(Tbl -> Tbl) -> FilePath -> IO ()
interactTable f = (putStr . mapTable f =<<) . getContentsFromFileOrStdin

cleanUpCell :: String -> String
-- cleanUpCell = reverse . dropWhile isSpace . reverse . dropWhile isSpace
cleanUpCell x | lx == "oui" = "OUI"
              | lx == "non" = "NON"
              | x  == "r\195\169gulier" = "R\195\169gulier"
              | otherwise              = x
  where lx = map toLower x

main :: IO ()
main = do
    args <- getArgs
    case args of
      [fileArg] -> interactTable (map . map $ cleanUpCell) fileArg
      -- we were expecting a single file name arg
      _ -> printSingleFileUsage
