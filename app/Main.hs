{-# LANGUAGE OverloadedStrings #-}

import Action ()
import Control.Lens
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import qualified Direction as D
import Draw
import Rule
import Stage
import System.Environment
import System.Exit
import Text.XML
import Text.XML.Lens
import Tile
import Util
import World

main :: IO ()
main = do
  -- let extract_csv_from_tmx = init . init . init . drop 5
  -- stage <- extract_csv_from_tmx . lines <$> readFile "stages/0.tmx"
  -- print (map init stage)

  doc <- Text.XML.readFile def "stages/0.tmx"
  let height = doc ^? root . named "map" ... named "layer" . attr "height"
  let width = doc ^? root . named "map" ... named "layer" . attr "width"
  let map = doc ^.. root . named "map" ... named "layer" ... named "data" . text
  print height
  print width
  print map

-- print width
-- print mydata

--   let objs = map OCharacter (generateEnumValues :: [Character])
--       tiles = map OTile (generateEnumValues :: [Tile])
--   objImages <- mapM loadIconImage (objs ++ tiles)

--   args <- getArgs
--   let stagePath = case args of
--         [] -> "stages/0.csv"
--         x : xs -> x
--   stage <- words <$> readFile stagePath
--   let worldSize = (\(x : y : _) -> (x, y)) $ map read $ splitOn "," $ head stage
--   let world = initWorld worldSize $ tail stage
--   playIO window black 24 world (drawWorld worldSize $ M.fromList objImages) handleEvent elapseWorld
