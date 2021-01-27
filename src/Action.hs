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
  ( ObjState (..),
    World (worldObjectsList),
    getRules,
    liftObjKind,
    worldObjects,
  )

walk :: D.Direction -> World -> World
walk d world = world {worldObjectsList = changeHead newObjects (worldObjectsList world)}
  where
    youList = getObjStatesWithComplement TYou (getRules world) (worldObjects world)
    movableList = nub $ concatMap (getMovableList (worldObjects world) (getRules world) d) youList
    samePositionList = worldObjects world \\ movableList
    changeYourDirection x = if x `elem` youList then changeDirection d x else x
    newObjects = map changeYourDirection $ samePositionList ++ map (stepObject d) movableList

changeDirection :: D.Direction -> ObjState -> ObjState
changeDirection d obj = obj {objStateDir = d}

stepObject :: D.Direction -> ObjState -> ObjState
stepObject d obj = obj {objStateX = newX, objStateY = newY, objStateDir = d}
  where
    (newX, newY) = updateXY (objStateX obj) (objStateY obj) d

data Collision = STOP | PUSH | THROUGH deriving (Eq)

getMovableList :: [ObjState] -> [Rule] -> D.Direction -> ObjState -> [ObjState]
getMovableList objects getRules dir you
  | canMove objects getRules dir (updateXY x y dir) = getMovableList' objects getRules dir [you]
  | otherwise = []
  where
    x = objStateX you
    y = objStateY you
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
        x = objStateX $ head pushedObjs
        y = objStateY $ head pushedObjs
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

    getObjectCollision :: [Rule] -> ObjState -> Collision
    getObjectCollision getRules obj =
      case (isPush, isStop) of
        (True, _) -> PUSH
        (False, True) -> STOP
        (False, False) -> THROUGH
      where
        isPush = obj `elem` getObjStatesWithComplement TPush getRules objects
        isStop = obj `elem` getObjStatesWithComplement TStop getRules objects

updateXY :: Int -> Int -> D.Direction -> (Int, Int)
updateXY x y D.Left = (x -1, y)
updateXY x y D.Down = (x, y -1)
updateXY x y D.Up = (x, y + 1)
updateXY x y D.Right = (x + 1, y)

findObjects :: [ObjState] -> (Int, Int) -> [ObjState]
findObjects objects (x, y) = filter (\obj -> x == objStateX obj && y == objStateY obj) objects

getObjStatesWithComplement :: Tile -> [Rule] -> [ObjState] -> [ObjState]
getObjStatesWithComplement c getRules = filter (\obj -> objStateKind obj `elem` objKindList)
  where
    subjects = getSubjects getRules c
    subjectsWithoutTile = subjects \\ [TText]
    objKindObjList = map (liftObjKind . tile2Character) subjectsWithoutTile
    objKindTileList = map liftObjKind tileSubjects
    objKindList = objKindObjList ++ objKindTileList
    tileSubjects = if TText `elem` subjects then allTiles else []
    allTiles = generateEnumValues :: [Tile]

win :: World -> Bool
win world = not (null (map obj2position youList `intersect` map obj2position winList))
  where
    youList = getObjStatesWithComplement TYou (getRules world) (worldObjects world)
    winList = getObjStatesWithComplement TWin (getRules world) (worldObjects world)
    obj2position obj = (objStateX obj, objStateY obj)

metamorphose :: World -> World
metamorphose world = world {worldObjectsList = changeHead (assignID $ removedObjs ++ metamonObjs) (worldObjectsList world)}
  where
    metamonRules = filter isMetamonRule (getRules world)
    isMetamonRule (Rule s v c) = s `elem` nounList && v == TIs && c `elem` nounList
    metamonObjs = concatMap (applyMetamon metamonRules) (worldObjects world)
    applyMetamon getRules obj = [obj {objStateKind = liftObjKind $ tile2Character (ruleC rule)} | rule <- getRules, liftObjKind (tile2Character (ruleS rule)) == objStateKind obj]
    removedObjs = filter (\x -> objStateKind x `notElem` map (liftObjKind . tile2Character . ruleS) metamonRules) (worldObjects world)

assignID :: [ObjState] -> [ObjState]
assignID objs = zipWith (\obj id -> obj {objStateId = id}) objs [1 ..]

getSubjects :: [Rule] -> Tile -> [Tile]
getSubjects getRules c = map ruleS $ filter (\rule -> ruleC rule == c) getRules
