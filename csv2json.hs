import Data.Functor
import Text.JSON
import Database.TxtSushi.FlatFile
import System.Environment
import Utils

{-
  Encoding note:
    Nothing fancy is done about encoding, on both ends.
    So it basically works for UTF-8 which does not mess
    up with the delimitors of both CSV and JSON.
 -}

csv2json :: FilePath -> IO JSValue
csv2json f = do
  tbl <- parseTable csvFormat <$> getContentsFromFileOrStdin f
  return $ JSArray (map (JSArray . map (JSString . toJSString)) (tbl :: [[String]]))

main = do args <- getArgs
          js <-
            case args of
              []  -> csv2json "-"
              [f] -> csv2json f
              fs  -> JSArray <$> mapM csv2json fs
          putStrLn . encode $ js

