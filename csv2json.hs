import Data.Functor
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L
import Database.TxtSushi.FlatFile
import System.Environment
import Utils

{-
  Encoding note:
    Nothing fancy is done about encoding, on both ends.
    So it basically works for UTF-8 which does not mess
    up with the delimitors of both CSV and JSON.
 -}

csv2json :: FilePath -> IO Value
csv2json =
  fmap (Array . fmap (Array . fmap (String . T.pack) . V.fromList)
              . V.fromList
              . parseTable csvFormat)
     . getContentsFromFileOrStdin

main :: IO ()
main = L.putStrLn . encode . mayArray
   =<< mapM csv2json . addDefaultInput
   =<< getArgs

  where mayArray [x] = x
        mayArray xs  = Array $ V.fromList xs
