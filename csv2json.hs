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
csv2json =
  fmap (JSArray . map (JSArray . map (JSString . toJSString))
                . parseTable csvFormat)
     . getContentsFromFileOrStdin

main :: IO ()
main = putStrLn . encode . mayJSArray
   =<< mapM csv2json . addDefaultInput
   =<< getArgs

  where mayJSArray [x] = x
        mayJSArray xs  = JSArray xs
