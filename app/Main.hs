{-# LANGUAGE OverloadedStrings #-}

import Action ()
import Control.Lens
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import qualified Direction as D
import Draw
import Rule
import Stage
import System.Environment
import System.Exit
import qualified Text.XML as X
import Text.XML.Lens
import Tile
import Util
import World
import Data.Aeson.Lens

main :: IO ()
main = do
  -- let extract_csv_from_tmx = init . init . init . drop 5
  -- stage <- extract_csv_from_tmx . lines <$> readFile "stages/0.tmx"
  -- print (map init stage)

  ----------------------------

  -- doc <- Text.XML.readFile def "stages/0.tmx"
  -- let Just height = (read . T.unpack <$> doc ^? root . named "map" ... named "layer" . attr "height") :: Maybe Int
  -- let Just width = (read . T.unpack <$> doc ^? root . named "map" ... named "layer" . attr "width") :: Maybe Int
  -- let Just stage = Data.List.filter (`notElem` ['\r','\n']) . T.unpack  <$> doc ^? root . named "map" ... named "layer" ... named "data" . text
  -- print height
  -- print width
  -- print ((map read $ splitOn "," stage)::[Int])

  ----------------------------

  tilemap <- readFile "stages/haskell_is_you.json"
  let ids = tilemap ^.. key "tiles" . values . key "id" . _Integer
  let imgs = tilemap ^.. key "tiles" . values . key "image" . _String

  -- let Just image_name_list = map (key "image") tilemap ^? key "tiles"
  -- let Just id_list = map (key "id") <$> tilemap ^? key "tiles"
  print $ zip imgs ids

-- Prelude.map (flip (^..) (key "id")) $ tilemap ^.. key "tiles" . value
-- tilemap ^.. key "tiles" . _Array . each . key "id" . _Integer
-- tilemap ^.. key "tiles" . values . key "id" . _Integer
  -- let Just id_list = map (key "id") <$> tilemap ^? key "tiles"
  print $ zip imgs ids

-- Prelude.map (flip (^..) (key "id")) $ tilemap ^.. key "tiles" . value
-- tilemap ^.. key "tiles" . _Array . each . key "id" . _Integer
-- tilemap ^.. key "tiles" . values . key "id" . _Integer

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
