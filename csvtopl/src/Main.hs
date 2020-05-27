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

type Gid = Int
type Coords = (Float, Float)

data Paragem = Paragem Gid Coords String String Bool String [Int] Int String String deriving Show

fromList :: [String] -> Maybe Paragem
fromList [gid, lat, long, econ, abr, pub, op, carr, crua, nrua, freg] = 
  let pgid = truncate ((read gid)::Double)
      plat = (read lat)::Float
      plong = (read long)::Float
      pcarr = map truncate ((read ("[" ++ carr ++ "]"))::[Double])
      pcrua = truncate ((read crua)::Double)
  in Just $ Paragem pgid (plat, plong) econ abr ((map toLower pub)=="yes") op pcarr pcrua nrua freg
fromList _ = Nothing

toProlog :: Paragem -> String
toProlog (Paragem gid (lat, long) ea tab pub op carr crua nrua freg)
  = concat
  [ "paragem("
  , show gid
  , ",(", "\n\t", "(" , show lat, ",", show long, ")"
  , "\n\t", ",('", ea, "','", tab, "',", map toLower . show $ pub, ",'", op, "')"
  , "\n\t", ",", show carr
  , "\n\t", ",(", show crua, ",'", nrua, "','", freg, "')"
  , "))."
  ]

toCellText :: CellValue -> Text.Text
toCellText (CellText txt) = txt
toCellText (CellDouble doub) = Text.pack . show $ doub
toCellText _ = ""

main :: IO ()
main = do
  bs <- L.readFile "docs/paragens.xlsx"
  let firstSheet = (!! 0) . map snd . view xlSheets . toXlsx $ bs
  let cells = view wsCells firstSheet
  --let x = atCell (1,1) (Just firstSheet)
  --putStrLn $ "Cell B3 contains " ++ 
  let lines = sequence . tail . map (sequence . map  snd) . groupBy ((/=) `on` (snd . fst)) . map (second (view cellValue)) $ Map.toList cells
  let linesStr = (map (map (Text.unpack . toCellText))) <$> lines
  let paragens = (catMaybes . map fromList) <$> linesStr
  case paragens of
    Just ps -> do 
      mapM_ (putStrLn . toProlog) ps
      pure ()
    Nothing -> pure ()

