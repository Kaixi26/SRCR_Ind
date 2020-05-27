{-# LANGUAGE OverloadedStrings #-}
module Main where
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Control.Lens
import Control.Arrow
import Data.Function
import Data.List
import Control.Monad
import qualified Data.Text as Text
import Data.Char
import Data.Maybe

data Adjacencia = Adjacencia Int [Int] deriving Show

toProlog :: Adjacencia -> String
toProlog (Adjacencia carr gids) = concat
  [ "adjacencia("
  , show carr
  , ", ", show gids
  , ")."
  ]

toCellText :: CellValue -> Text.Text
toCellText (CellText txt) = txt
toCellText (CellDouble doub) = Text.pack . show $ doub
toCellText _ = ""

parseSheet (name, wsheet) = 
  let mcells = view wsCells wsheet
      gids = map (\x -> truncate ((read x)::Double)) $
        map (Text.unpack . toCellText) $ 
        catMaybes $ map (view cellValue) $ 
        catMaybes $ takeWhile isJust [ Map.lookup (line, 1) mcells | line<-[2..]]
  in Adjacencia (read $ Text.unpack $ name) gids

main :: IO ()
main = do
  bs <- L.readFile "docs/adjacencias.xlsx"
  let sheets = view xlSheets . toXlsx $ bs
  mapM_ putStrLn $ map (toProlog . parseSheet) sheets
  pure ()

