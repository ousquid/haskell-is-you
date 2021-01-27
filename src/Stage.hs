module Stage
  ( initWorld,
  )
where

import Data.List.Split
import qualified Data.Map as M
import qualified Direction as D
import Draw
import Graphics.Gloss
import Text
import World

initWorld :: (WorldWidth, WorldHeight) -> [String] -> World
initWorld worldSize stage =
  let objStates = map (stringToObjState . splitOn ",") stage
  in World {worldObjectsList = [zipWith (\g x -> g x) objStates [1 ..] ++ voidObjects worldSize]}

voidObjects :: (WorldWidth, WorldHeight) -> [ObjState]
voidObjects (width, height) = leftAndRight ++ aboveAndBottom
  where
    leftAndRight = [ObjState x y D.Down (ObjKindObj CVoid) False 0 | x <- [-1, width], y <- [0 .. height -1]]
    aboveAndBottom = [ObjState x y D.Down (ObjKindObj CVoid) False 0 | x <- [0 .. width -1], y <- [-1, height]]

stringToObjState :: [String] -> (Int -> ObjState)
stringToObjState (x : y : dir : kind : _) = ObjState _x _y _dir objKind isText
  where
    isText = head kind == 'T'
    _x = read x :: Int
    _y = read y :: Int
    _dir = read dir :: D.Direction
    objKind = if isText then liftObjKind (read kind :: Text) else liftObjKind (read kind :: Character)
