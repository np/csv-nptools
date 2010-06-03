module Utils where

import Database.TxtSushi.FlatFile
import Data.Char
import Debug.Trace

type Tbl = [[String]]

mapTable ::(Tbl -> Tbl) -> String -> String
mapTable f = formatTable csvFormat . f . parseTable csvFormat

getContentsFromFileOrStdin :: FilePath -> IO String
getContentsFromFileOrStdin "-"  = getContents
getContentsFromFileOrStdin fp   = readFile fp

interactTable ::(Tbl -> Tbl) -> FilePath -> IO ()
interactTable f = (putStr . mapTable f =<<) . getContentsFromFileOrStdin

evalChar :: Char -> Char
evalChar 'a'   = '\a'
evalChar 'b'   = '\b'
evalChar 'f'   = '\f'
evalChar 'n'   = '\n'
evalChar 'r'   = '\r'
evalChar 't'   = '\t'
evalChar 'v'   = '\v'
evalChar x     = x

evalStr :: String -> String
evalStr ('\\':'0':'x':x:y:xs)  = chr (16 * digitToInt x + digitToInt y) : evalStr xs
evalStr ('\\':x:xs)            = evalChar x  : evalStr xs
evalStr (x:xs)                 = x           : evalStr xs
evalStr []                     = []

