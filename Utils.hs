module Utils where

import System.IO
import Database.TxtSushi.FlatFile
import Data.Char
-- import Debug.Trace

type Tbl = [[String]]

mapTable ::(Tbl -> Tbl) -> String -> String
mapTable f = formatTable csvFormat . f . parseTable csvFormat

padTbl :: a -> [[a]] -> [[a]]
padTbl _     [] = []
padTbl dflt  xss0@(xs0:_)
  = zipWith (padRow cols) [1::Int ..] $ xss0
  where cols = length xs0
        -- padRow = take cols (xs ++ repeat dflt)
        padRow 0 _ []      = []
        padRow 0 i _       = error $ "Row " ++ show i ++ " is too long"
        padRow n _ []      = replicate n dflt
        padRow n i (x:xs)  = x : padRow (n - 1) i xs

getContentsFromFileOrStdin :: FilePath -> IO String
getContentsFromFileOrStdin "-"  = hSetEncoding stdin latin1 >> getContents
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

expandRanges :: String -> String
expandRanges (x:'-':y:zs)  = [x..y]++expandRanges zs
expandRanges (x:xs)        = x : expandRanges xs
expandRanges []            = []
