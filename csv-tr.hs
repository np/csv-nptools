import System.Environment
import Utils

csvTrCell :: String -> String -> (String -> String)
csvTrCell src dst = foldr ((.).map) id $ zipWith' err substChar src dst
  where err = error "Source and destination should have the same length"

csvTr :: String -> String -> FilePath -> IO ()
csvTr src dst = interactTable (map . map $ csvTrCell (onStr src) (onStr dst))
  where onStr = expandRanges . evalStr

main :: IO ()
main = do
    args <- getArgs
    case args of
      src : dst : xs -> csvTr src dst (getInput err xs)
      _              -> err

  where err = error $ unlines  ["Usage: csv-tr <src> <dst> [<file>|-]"
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
