{-# LANGUAGE OverloadedStrings #-}

import Action ()
import Control.Lens
import Data.Aeson.Lens
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Debug.Trace
import qualified Direction as D
import Draw
import Rule
import Stage
import System.Environment
import System.Exit
import System.FilePath.Posix
import qualified Text.XML as X
import Text.XML.Lens
import Tile
import Util
import World

parseImagePath :: T.Text -> (Icon, D.Direction)
parseImagePath str = (icon, dir)
  where
    [iconStr, directionStr] = splitOn "_" $ takeBaseName $ T.unpack str
    icon = if head iconStr == 'T' then OTile (read iconStr :: Tile) else OCharacter (read iconStr :: Character)
    dir = read directionStr :: D.Direction

main :: IO ()
main = do
  -- let extract_csv_from_tmx = init . init . init . drop 5
  -- stage <- extract_csv_from_tmx . lines <$> readFile "stages/0.tmx"
  -- print (map init stage)

  ----------------------------

  doc <- X.readFile X.def "stages/0.tmx"
  let Just height = (read . T.unpack <$> doc ^? root . named "map" ... named "layer" . attr "height") :: Maybe Int
  let Just width = (read . T.unpack <$> doc ^? root . named "map" ... named "layer" . attr "width") :: Maybe Int
  let Just stage = Data.List.filter (`notElem` ['\r', '\n']) . T.unpack <$> doc ^? root . named "map" ... named "layer" ... named "data" . text
  print height
  print width
  let stageInt = (map read $ splitOn "," stage) :: [Int]
  print [((idx `mod` width, idx `div` width), id) | (idx, id) <- zip [0 ..] stageInt, id /= 0]

----------------------------

-- tilemap <- readFile "stages/haskell_is_you.json"
-- let ids = tilemap ^.. key "tiles" . values . key "id" . _Integer
-- let imgs = tilemap ^.. key "tiles" . values . key "image" . _String

-- print $ zip ids $ map parseImagePath imgs

----------------------------

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
