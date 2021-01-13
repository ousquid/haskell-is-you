import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import qualified Direction as D
import Draw
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Keyboard
import Rule
import Stage
import System.Exit
import World

-------------------
-- Display の設定
-------------------

window :: Display
window = InWindow "Haskell Is You" (windowWidth, windowHeight) (0, 0)

--------------------------
-- シミュレーションの実装
--------------------------

text2Object :: Text -> Object
text2Object text = read $ "O" ++ (drop 1 $ show text)

applyHead :: (a -> a) -> [a] -> [a]
applyHead f (x : xs) = (f x) : xs

changeHead :: a -> [a] -> [a]
changeHead y (x : xs) = y : xs

-----------------------------------
-- updateWorld関連
-----------------------------------

win :: World -> Bool
win world = not (null (map obj2position youList `intersect` map obj2position winList))
  where
    youList = getObjStatesWithComplement TYou (getRules world) (worldObjects world)
    winList = getObjStatesWithComplement TWin (getRules world) (worldObjects world)
    obj2position obj = (objStateX obj, objStateY obj)

-- | イベントを処理する関数。EventKey以外のイベントは無視する
handleEvent :: Event -> World -> IO World
handleEvent (EventKey key Down _ _) world = do
  let w = updateWorld key world
  print (getRules w)
  if win w then exitSuccess else return w
handleEvent _ world = return world

updateWorld :: Key -> World -> World
updateWorld key world = case action of
  Move dir -> removeUncangedWorldObjects $ metamorphose $ walk dir $ duplicateWorldObjects world
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
  | otherwise = world {worldObjectsList = (head $ worldObjectsList world) : (worldObjectsList world)}
duplicateWorldObjects world = world {worldObjectsList = (head $ worldObjectsList world) : (worldObjectsList world)}

tailWorldObjects :: World -> World
tailWorldObjects world@(World (x : [])) = world
tailWorldObjects world = world {worldObjectsList = tail $ worldObjectsList world}

assignID :: [ObjState] -> [ObjState]
assignID objs = zipWith (\obj id -> obj {objStateId = id}) objs [1 ..]

metamorphose :: World -> World
metamorphose world = world {worldObjectsList = changeHead (assignID $ removedObjs ++ metamonObjs) (worldObjectsList world)}
  where
    metamonRules = filter isMetamonRule (getRules world)
    isMetamonRule (Rule s v c) = s `elem` nounList && v == TIs && c `elem` nounList
    metamonObjs = concatMap (applyMetamon metamonRules) (worldObjects world)
    applyMetamon getRules obj = [obj {objStateKind = liftObjState $ text2Object (ruleC rule)} | rule <- getRules, liftObjState (text2Object (ruleS rule)) == objStateKind obj]
    removedObjs = filter (\x -> objStateKind x `notElem` map (liftObjState . text2Object . ruleS) metamonRules) (worldObjects world)

walk :: D.Direction -> World -> World
walk d world = world {worldObjectsList = changeHead newObjects (worldObjectsList world)}
  where
    youList = getObjStatesWithComplement TYou (getRules world) (worldObjects world)
    movableList = nub $ concatMap (getMovableList (worldObjects world) (getRules world) d) youList
    unmovableList = (worldObjects world) \\ movableList
    newObjects = unmovableList ++ (map (stepObject d) movableList)

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

getSubjects :: [Rule] -> Text -> [Text]
getSubjects getRules c = map ruleS $ filter (\rule -> (ruleC rule) == c) getRules

getObjStatesWithComplement :: Text -> [Rule] -> [ObjState] -> [ObjState]
getObjStatesWithComplement c getRules objects = filter (\obj -> objStateKind obj `elem` objKindList) objects
  where
    subjects = getSubjects getRules c
    subjectsWithoutText = subjects \\ [TText]
    objKindObjList = map (liftObjState . text2Object) subjectsWithoutText
    objKindTextList = map liftObjState textSubjects
    objKindList = objKindObjList ++ objKindTextList
    textSubjects = if TText `elem` subjects then allTexts else []
    allTexts = generateEnumValues :: [Text]

updateXY :: Int -> Int -> D.Direction -> (Int, Int)
updateXY x y D.Left = (x -1, y)
updateXY x y D.Down = (x, y -1)
updateXY x y D.Up = (x, y + 1)
updateXY x y D.Right = (x + 1, y)

findObjects :: [ObjState] -> (Int, Int) -> [ObjState]
findObjects objects (x, y) = filter (\obj -> x == (objStateX obj) && y == (objStateY obj)) objects

-----------------------------------
-- nextWorld関連
-----------------------------------
elapseWorld :: Float -> World -> IO World
elapseWorld dt world = return world

-----------------------------------
-- initWorld関連
-----------------------------------
generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)

-----------------------------------
-- main 関数
-----------------------------------

main :: IO ()
main = do
  let objs = map liftObjState (generateEnumValues :: [Object])
      texts = map liftObjState (generateEnumValues :: [Text])
  objImages <- mapM loadObjImage (objs ++ texts)
  let world = initWorld -- obj_images
  let worldSize = (33, 18)
  playIO window black 24 world (drawWorld worldSize $ M.fromList objImages) handleEvent elapseWorld
