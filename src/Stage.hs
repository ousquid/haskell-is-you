module Stage
  ( initWorld,
  )
where

import Data.List.Split
import qualified Data.Map as M
import qualified Direction as D
import Draw
import Graphics.Gloss
import Tile
import World

initWorld :: (WorldWidth, WorldHeight) -> [String] -> World
initWorld worldSize stage =
  let objects = map (stringToObject . splitOn ",") stage
   in World {worldObjectsList = [zipWith (\g x -> g x) objects [1 ..] ++ voidObjects worldSize]}

voidObjects :: (WorldWidth, WorldHeight) -> [Object]
voidObjects (width, height) = leftAndRight ++ aboveAndBottom
  where
    leftAndRight = [Object x y D.Down (OCharacter CVoid) 0 | x <- [-1, width], y <- [0 .. height -1]]
    aboveAndBottom = [Object x y D.Down (OCharacter CVoid) 0 | x <- [0 .. width -1], y <- [-1, height]]

stringToObject :: [String] -> (Int -> Object)
stringToObject (x : y : dir : kind : _) = Object _x _y _dir objKind
  where
    isTile = head kind == 'T'
    _x = read x :: Int
    _y = read y :: Int
    _dir = read dir :: D.Direction
    objKind = if isTile then liftObject (read kind :: Tile) else liftObject (read kind :: Character)
