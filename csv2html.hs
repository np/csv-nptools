import Text.Tabular
import qualified Text.Tabular.Html as Tab
import Text.CSV
import qualified Text.Html as H
import System.Environment

{- <<< -}
simpleHeader :: Properties -> [a] -> Header a
simpleHeader p = Group p . map Header

enumHeaderFromTo :: Enum a => Properties -> a -> a -> Header a
enumHeaderFromTo p from to = Group p $ map Header [from..to]

simpleTable :: Properties -> Properties -> [[String]] -> Table Int String String
simpleTable _  _  [] = error "no header"
simpleTable ph pv (hs:tbl) = Table rh ch tbl
  where height = length tbl
        rh = enumHeaderFromTo ph 1 height
        ch = simpleHeader pv hs
{- >>> -}

main :: IO ()
main = putStr . H.renderHtml . layout . tabular2html . csv2tabular =<< parse =<< getArgs
            
  where tabular2html  = Tab.render (H.toHtml . show) H.toHtml H.toHtml
        parse [arg]   = either (fail . show) return =<< parseCSVFromFile arg
        parse _       = fail "Usage: csv2html <file.csv>"
        csv2tabular   = simpleTable SingleLine SingleLine
        layout b      = H.header (charset H.+++ Tab.css Tab.defaultCss) H.+++ H.body b
        charset       = H.meta H.! [H.httpequiv "Content-type", H.content "text/html; charset=UTF-8"]
