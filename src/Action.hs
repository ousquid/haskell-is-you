module Action
  ( walk,
    metamorphose,
    win,
  )
where

import Data.List
import qualified Direction as D
import Rule (Rule (Rule, ruleC, ruleS), nounList)
import Tile (Tile (..))
import Util (changeHead, generateEnumValues, tile2Character)
import World
  ( Object (..),
    World (worldObjectsList),
    getRules,
    Icon (..),
    worldObjects,
  )

walk :: D.Direction -> World -> World
walk d world = world {worldObjectsList = changeHead newObjects (worldObjectsList world)}
  where
    youList = getObjectsWithComplement TYou (getRules world) (worldObjects world)
    movableList = nub $ concatMap (getMovableList (worldObjects world) (getRules world) d) youList
    samePositionList = worldObjects world \\ movableList
    changeYourDirection x = if x `elem` youList then changeDirection d x else x
    newObjects = map changeYourDirection $ samePositionList ++ map (stepObject d) movableList

changeDirection :: D.Direction -> Object -> Object
changeDirection d obj = obj {objectDir = d}

stepObject :: D.Direction -> Object -> Object
stepObject d obj = obj {objectX = newX, objectY = newY, objectDir = d}
  where
    (newX, newY) = updateXY (objectX obj) (objectY obj) d

data Collision = STOP | PUSH | THROUGH deriving (Eq)

getMovableList :: [Object] -> [Rule] -> D.Direction -> Object -> [Object]
getMovableList objects getRules dir you
  | canMove objects getRules dir (updateXY x y dir) = getMovableList' objects getRules dir [you]
  | otherwise = []
  where
    x = objectX you
    y = objectY you
    canMove objects getRules dir (x, y) =
      case getCellCollision objects getRules (x, y) of
        PUSH -> canMove objects getRules dir $ updateXY x y dir
        STOP -> False
        THROUGH -> True
    getMovableList' objects getRules dir pushedObjs =
      case collisionState of
        PUSH -> pushedObjs ++ movableList
        THROUGH -> pushedObjs
      where
        x = objectX $ head pushedObjs
        y = objectY $ head pushedObjs
        newPos = updateXY x y dir
        movableList = getMovableList' objects getRules dir pushList
        pushList = filter (\obj -> getObjectCollision getRules obj == PUSH) $ findObjects objects newPos
        collisionState = getCellCollision objects getRules newPos

    getCellCollision objects getRules pos
      | STOP `elem` objectStates = STOP
      | PUSH `elem` objectStates = PUSH
      | otherwise = THROUGH
      where
        objectStates = map (getObjectCollision getRules) $ findObjects objects pos

    getObjectCollision :: [Rule] -> Object -> Collision
    getObjectCollision getRules obj =
      case (isPush, isStop) of
        (True, _) -> PUSH
        (False, True) -> STOP
        (False, False) -> THROUGH
      where
        isPush = obj `elem` getObjectsWithComplement TPush getRules objects
        isStop = obj `elem` getObjectsWithComplement TStop getRules objects

updateXY :: Int -> Int -> D.Direction -> (Int, Int)
updateXY x y D.Left = (x -1, y)
updateXY x y D.Down = (x, y -1)
updateXY x y D.Up = (x, y + 1)
updateXY x y D.Right = (x + 1, y)

findObjects :: [Object] -> (Int, Int) -> [Object]
findObjects objects (x, y) = filter (\obj -> x == objectX obj && y == objectY obj) objects

getObjectsWithComplement :: Tile -> [Rule] -> [Object] -> [Object]
getObjectsWithComplement c getRules = filter (\obj -> objectIcon obj `elem` objKindList)
  where
    subjects = getSubjects getRules c
    subjectsWithoutTile = subjects \\ [TText]
    objKindObjList = map (OCharacter . tile2Character) subjectsWithoutTile
    objKindTileList = map OTile tileSubjects
    objKindList = objKindObjList ++ objKindTileList
    tileSubjects = if TText `elem` subjects then allTiles else []
    allTiles = generateEnumValues :: [Tile]

win :: World -> Bool
win world = not (null (map obj2position youList `intersect` map obj2position winList))
  where
    youList = getObjectsWithComplement TYou (getRules world) (worldObjects world)
    winList = getObjectsWithComplement TWin (getRules world) (worldObjects world)
    obj2position obj = (objectX obj, objectY obj)

metamorphose :: World -> World
metamorphose world = world {worldObjectsList = changeHead (assignID $ removedObjs ++ metamonObjs) (worldObjectsList world)}
  where
    metamonRules = filter isMetamonRule (getRules world)
    isMetamonRule (Rule s v c) = s `elem` nounList && v == TIs && c `elem` nounList
    metamonObjs = concatMap (applyMetamon metamonRules) (worldObjects world)
    applyMetamon getRules obj = [obj {objectIcon = OCharacter $ tile2Character (ruleC rule)} | rule <- getRules, OCharacter (tile2Character (ruleS rule)) == objectIcon obj]
    removedObjs = filter (\x -> objectIcon x `notElem` map (OCharacter . tile2Character . ruleS) metamonRules) (worldObjects world)

assignID :: [Object] -> [Object]
assignID objs = zipWith (\obj id -> obj {objectId = id}) objs [1 ..]

getSubjects :: [Rule] -> Tile -> [Tile]
getSubjects getRules c = map ruleS $ filter (\rule -> ruleC rule == c) getRules
