import Control.Arrow
import System.Environment
import System.Console.GetOpt
import Database.TxtSushi.FlatFile
import Utils

reformat :: (String, String) -> FilePath -> IO ()
reformat (inDelim, outDelim)
   = (putStr . formatTable outFormat . parseTable inFormat =<<) . getContentsFromFileOrStdin
  where inFormat        = mkFormat inDelim 
        outFormat       = mkFormat outDelim
        mkFormat delim  = Format "\"" delim ["\n","\r","\r\n","\n\r"]

type Settings = (String, String)
type Flag = Settings -> Settings

options :: [OptDescr Flag]
options =
  [ Option "d"  [] (ReqArg (first   . const . evalStr) "STRING") "Input delim (default ',')"
  , Option "D"  [] (ReqArg (second  . const . evalStr) "STRING") "Output delim (default 0xfe)"
  ]

usage :: String -> a
usage msg = error $ unlines [msg, usageInfo header options]
  where header = "Usage: csv-reformat [-d <input-delim>] [-D <output-delim>] [<file-or-dash>]"

main :: IO ()
main = do
    args <- getArgs
    let (flags, nonopts, errs) = getOpt Permute options args
    let opts = foldr ($) (",", "\254") flags
    case (nonopts, errs) of
      ([fileArg], []) -> reformat opts fileArg
      ([],        []) -> reformat opts "-"
      (_,          _) -> usage (concat errs)
