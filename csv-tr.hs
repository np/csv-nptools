import System.Environment
import Utils

substChar :: Eq a => a -> a -> (a -> a)
substChar s d x  | x == s     = d
                 | otherwise  = x

csvTrCell :: String -> String -> (String -> String)
csvTrCell []       []       = id
csvTrCell (s:src)  (d:dst)  = csvTrCell src dst . map (substChar s d)
csvTrCell _        _        = error "Source and destination should have the same length"

csvTr :: String -> String -> FilePath -> IO ()
csvTr src dst = interactTable (map . map $ csvTrCell (onStr src) (onStr dst))
  where onStr = expandRanges . evalStr

main :: IO ()
main = do
    args <- getArgs
    case args of
      [src, dst, fileArg]  -> csvTr src dst fileArg
      [src, dst]           -> csvTr src dst "-"
      _ -> error $ unlines  ["Usage: csv-tr <src> <dst> [<file-or-dash>]"
                            ,"Examples: csv-tr ',\\n\\t' '...'"
                            ,"          csv-tr a-z A-Z"
                            ,"Characters:"
                            ,"\\a    <alert character>"
                            ,"\\b    <backspace>"
                            ,"\\f    <form-feed>"
                            ,"\\n    <newline>"
                            ,"\\r    <carriage return>"
                            ,"\\t    <tab>"
                            ,"\\v    <vertical tab>"
                            ]
