{-# LANGUAGE OverloadedStrings #-}

import Action
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
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Keyboard
import Rule
import Stage
import System.Environment
import System.Exit
import qualified Text.XML as X
import qualified Text.XML.Lens as XL
import Tile
import Util
import World

handleEvent :: Event -> World -> IO World
handleEvent (EventKey key Down _ _) world = do
  let w = updateWorld key world
  print (getRules w)
  if win w then exitSuccess else return w
handleEvent _ world = return world

updateWorld :: Key -> World -> World
updateWorld key world = case action of
  Move dir -> removeUncangedWorldObjects $ defeat $ melt $ open $ sink $ metamorphose $ walk dir $ duplicateWorldObjects world
  Step -> metamorphose $ duplicateWorldObjects world
  Reverse -> tailWorldObjects world
  DoNothing -> world
  where
    action = keyToAction key

removeUncangedWorldObjects :: World -> World
removeUncangedWorldObjects world@(World (x : y : ys))
  | x == y = world {worldObjectsList = y : ys}
  | otherwise = world

duplicateWorldObjects :: World -> World
duplicateWorldObjects world@(World (x : y : _))
  | x == y = world
  | otherwise = world {worldObjectsList = head (worldObjectsList world) : worldObjectsList world}
duplicateWorldObjects world = world {worldObjectsList = head (worldObjectsList world) : worldObjectsList world}

tailWorldObjects :: World -> World
tailWorldObjects world@(World [x]) = world
tailWorldObjects world = world {worldObjectsList = tail $ worldObjectsList world}

window :: Display
window = InWindow "Haskell Is You" (windowWidth, windowHeight) (0, 0)

elapseWorld :: Float -> World -> IO World
elapseWorld dt = return

main :: IO ()
main = do
  let objs = map OCharacter (generateEnumValues :: [Character])
      tiles = map OTile (generateEnumValues :: [Tile])
  objImages <- mapM loadIconImage (objs ++ tiles)

  args <- getArgs
  let stagePath = case args of
        [] -> "stages/0.tmx"
        x : xs -> x

  -- ステージファイル tmx の読み込み
  doc <- X.readFile X.def stagePath
  let Just width = (read . T.unpack <$> doc ^? XL.root . XL.named "map" ... XL.named "layer" . XL.attr "width") :: Maybe Int
  let Just height = (read . T.unpack <$> doc ^? XL.root . XL.named "map" ... XL.named "layer" . XL.attr "height") :: Maybe Int
  let Just stageStr = Data.List.filter (`notElem` ['\r', '\n']) . T.unpack <$> doc ^? XL.root . XL.named "map" ... XL.named "layer" ... XL.named "data" . XL.text
  -- let stage = (map read $ splitOn "," stageStr) :: [Integer]

  -- タイルマップ json の読み込み
  tilemap <- readFile "stages/haskell_is_you.json"
  let tileIds = tilemap ^.. key "tiles" . values . key "id" . _Integer
  let tileImgPaths = tilemap ^.. key "tiles" . values . key "image" . _String

  let world = initWorld (width, height) stageStr tileIds tileImgPaths
  playIO window black 24 world (drawWorld (width, height) $ M.fromList objImages) handleEvent elapseWorld
