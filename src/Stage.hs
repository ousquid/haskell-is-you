module Stage
  ( initWorld,
  )
where

import qualified Data.Map as M
import qualified Direction as D
import Draw
import Graphics.Gloss
import World
import Data.List.Split

initWorld :: [String] -> World
initWorld stage =
  let objStates = map (stringToObjState . splitOn ",") stage
  in World { worldObjectsList = [zipWith (\g x -> g x) objStates [1 ..]] }

stringToObjState :: [String] -> (Int -> ObjState)
stringToObjState (x:y:dir:kind:_) = ObjState _x _y _dir objKind isText
  where isText = head kind == 'T'
        _x = read x::Int 
        _y = read y::Int
        _dir = read dir::D.Direction 
        objKind = if isText then liftObjKind (read kind::Text) else liftObjKind (read kind::Object)
