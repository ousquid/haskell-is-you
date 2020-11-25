module Main where

import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy

-------------------
-- Display の設定
-------------------

window :: (Int, Int) -> Display
window (w, h) = InWindow "Hello World" (w * objWidth, h * objHeight) (0, 0)

--------------------------
-- シミュレーションの実装
--------------------------

objWidth, objHeight :: Num a => a
objWidth = 64
objHeight = 64

objImgScale :: Float
objImgScale = objWidth / 320

type WorldHeight = Int

type WorldWidth = Int

type PictureLeft = Picture

type PictureDown = Picture

type PictureUp = Picture

type PictureRight = Picture

data World = World
  { gridLinePicture :: Picture,
    imageMap :: M.Map ObjKind (PictureLeft, PictureDown, PictureUp, PictureRight),
    worldObjects :: [ObjState],
    worldSize :: (WorldWidth, WorldHeight),
    rules :: [Rule]
  }

data Rule = Rule
  { ruleS :: Text,
    ruleV :: Text,
    ruleC :: Text
  }

instance Show Rule where
  show (Rule s v c) = " " ++ (intercalate " " $ map (tail . show) [s, v, c])

data Direction = ObjLeft | ObjDown | ObjUp | ObjRight deriving (Show, Eq)

data ObjState = ObjState
  { objStateX :: Int,
    objStateY :: Int,
    objStateDir :: Direction,
    objStateKind :: ObjKind,
    objStateIText :: Bool,
    objStateId :: Int
  }
  deriving (Show, Eq)

class ObjKindInterface a where
  liftObjState :: a -> ObjKind

instance ObjKindInterface Text where
  liftObjState text = ObjKindText text

instance ObjKindInterface Object where
  liftObjState obj = ObjKindObj obj

data ObjKind = ObjKindText Text | ObjKindObj Object deriving (Eq, Show, Ord)

data Text = TText | THaskell | TRock | TWall | TFlag | TWin | TStop | TPush | TIs | TYou deriving (Eq, Show, Enum, Ord, Read)

data Object = OHaskell | ORock | OWall | OFlag deriving (Eq, Show, Enum, Ord, Read)

text2Object :: Text -> Object
text2Object text = read $ "O" ++ (drop 1 $ show text)

-----------------------------------
-- drawWorld関連
-----------------------------------

drawObj :: (WorldWidth, WorldHeight) -> ObjState -> Picture -> Picture
drawObj (width, height) obj picture = translate ((fromIntegral $ (objStateX obj) - width `div` 2) * objWidth) ((fromIntegral $ (objStateY obj) - height `div` 2) * objHeight) $ scale objImgScale objImgScale picture

pickPicture :: (Picture, Picture, Picture, Picture) -> Direction -> Picture
pickPicture (x, _, _, _) ObjLeft = x
pickPicture (_, x, _, _) ObjDown = x
pickPicture (_, _, x, _) ObjUp = x
pickPicture (_, _, _, x) ObjRight = x

drawWorld :: World -> IO Picture
drawWorld world = do
  --    let objPictures = [drawObj (worldSize world) obj $ pickPicture ((imageMap world)!!(fromEnum $ objStateKind obj)) (objStateDir obj) | obj <- worldObjects world]
  let objPictures = [drawObj (worldSize world) obj $ pickPicture ((imageMap world) M.! (objStateKind obj)) (objStateDir obj) | obj <- worldObjects world]
  return (pictures (objPictures ++ [gridLinePicture world]))

-----------------------------------
-- updateWorld関連
-----------------------------------

-- | イベントを処理する関数。EventKey以外のイベントは無視する
updateWorld :: Event -> World -> IO World
updateWorld (EventKey key Down _ _) world = do
  let w = updateWorldWithKey key world
  putStrLn $ show (rules w)
  return w
updateWorld _ world = return world

updateWorldWithKey :: Key -> World -> World
updateWorldWithKey key world = case (getDirection key) of
  Just dir -> newWorld
    where
      newWorld = updateRule $ walk dir world
  Nothing -> world

defaultRule :: [Rule]
defaultRule = [Rule {ruleS = TText, ruleV = TIs, ruleC = TPush}]

updateRule :: World -> World
updateRule world = world {rules = concatMap (getRules texts) is_list ++ defaultRule}
  where
    is_list :: [ObjState]
    is_list = filter (\obj -> (objStateKind obj) == (ObjKindText TIs)) (worldObjects world)

    getText :: ObjState -> [ObjState] -- ObjStateがTextならそのものを、ObjStateがObjectなら空を返す
    getText obj = case (objStateKind obj) of
      ObjKindText _ -> [obj]
      ObjKindObj _ -> []
    texts = concatMap getText (worldObjects world)
    getRules :: [ObjState] -> ObjState -> [Rule]
    getRules texts is = verticalRule ++ horizontalRule
      where
        x = objStateX is
        y = objStateY is
        upObj = findText texts (x, y + 1)
        downObj = findText texts (x, y -1)
        verticalRule = createRule upObj downObj
        leftObj = findText texts (x -1, y)
        rightObj = findText texts (x + 1, y)
        horizontalRule = createRule leftObj rightObj

        createRule sObj cObj = case (sObj, cObj) of
          (Just a, Just b) -> [Rule {ruleS = a, ruleV = TIs, ruleC = b}]
          otherwise -> []
        findText :: [ObjState] -> (Int, Int) -> Maybe Text
        findText objects (x, y) = do
          obj <- findObject objects (x, y)
          let ObjKindText txt = objStateKind obj
          return txt

walk :: Direction -> World -> World
walk d world = world {worldObjects = unmovableList ++ (map (stepObject d) movableList)}
  where
    youList = getObjStatesWithComplement TYou (rules world) (worldObjects world)
    movableList = nub $ concatMap (getMovableList (worldObjects world) (rules world) d) youList
    unmovableList = (worldObjects world) \\ movableList

stepObject :: Direction -> ObjState -> ObjState
stepObject d obj = obj {objStateX = newX, objStateY = newY, objStateDir = d}
  where
    (newX, newY) = updateXY (objStateX obj) (objStateY obj) d

data Collision = STOP | PUSH | THROUGH deriving (Eq)

getMovableList :: [ObjState] -> [Rule] -> Direction -> ObjState -> [ObjState]
getMovableList objects rules dir you
  | canMove objects rules dir (updateXY x y dir) = getMovableList' objects rules dir [you]
  | otherwise = []
  where
    x = objStateX you
    y = objStateY you
    canMove objects rules dir (x, y) =
      case getCellCollision objects rules (x, y) of
        PUSH -> canMove objects rules dir $ updateXY x y dir
        STOP -> False
        THROUGH -> True
    getMovableList' objects rules dir pushedObjs =
      case collisionState of
        PUSH -> pushedObjs ++ movableList
        THROUGH -> pushedObjs
      where
        x = objStateX $ head pushedObjs
        y = objStateY $ head pushedObjs
        newPos = updateXY x y dir
        movableList = getMovableList' objects rules dir pushList
        pushList = filter (\obj -> getObjectCollision rules obj == PUSH) $ findObjects objects newPos
        collisionState = getCellCollision objects rules newPos

    getCellCollision objects rules pos
      | STOP `elem` objectStates = STOP
      | PUSH `elem` objectStates = PUSH
      | otherwise = THROUGH
      where
        objectStates = map (getObjectCollision rules) $ findObjects objects pos

    getObjectCollision :: [Rule] -> ObjState -> Collision
    getObjectCollision rules obj =
      case (isPush, isStop) of
        (True, _) -> PUSH
        (False, True) -> STOP
        (False, False) -> THROUGH
      where
        isPush = obj `elem` getObjStatesWithComplement TPush rules objects
        isStop = obj `elem` getObjStatesWithComplement TStop rules objects

getSubjects :: [Rule] -> Text -> [Text]
getSubjects rules c = map ruleS $ filter (\rule -> (ruleC rule) == c) rules

getObjStatesWithComplement :: Text -> [Rule] -> [ObjState] -> [ObjState]
getObjStatesWithComplement c rules objects = filter (\obj -> objStateKind obj `elem` objKindList) objects
  where
    subjects = getSubjects rules c
    subjectsWithoutText = subjects \\ [TText]
    objKindObjList = map (liftObjState . text2Object) subjectsWithoutText
    objKindTextList = map liftObjState textSubjects
    objKindList = objKindObjList ++ objKindTextList
    textSubjects = if TText `elem` subjects then allTexts else []
    allTexts = generateEnumValues :: [Text]

updateXY :: Int -> Int -> Direction -> (Int, Int)
updateXY x y ObjLeft = (x -1, y)
updateXY x y ObjDown = (x, y -1)
updateXY x y ObjUp = (x, y + 1)
updateXY x y ObjRight = (x + 1, y)

findObject :: [ObjState] -> (Int, Int) -> Maybe ObjState
findObject objects (x, y) = find (\obj -> x == (objStateX obj) && y == (objStateY obj)) objects

findObjects :: [ObjState] -> (Int, Int) -> [ObjState]
findObjects objects (x, y) = filter (\obj -> x == (objStateX obj) && y == (objStateY obj)) objects

-- | 方向キーとWASDキーに対応して四角形を移動させる
getDirection :: Key -> Maybe Direction
getDirection (SpecialKey KeyLeft) = Just ObjLeft
getDirection (SpecialKey KeyDown) = Just ObjDown
getDirection (SpecialKey KeyUp) = Just ObjUp
getDirection (SpecialKey KeyRight) = Just ObjRight
getDirection (Char 'a') = Just ObjLeft
getDirection (Char 's') = Just ObjDown
getDirection (Char 'w') = Just ObjUp
getDirection (Char 'd') = Just ObjRight
getDirection _ = Nothing

-----------------------------------
-- nextWorld関連
-----------------------------------
nextBox :: Float -> ObjState -> ObjState
nextBox dt box =
  let x = objStateX box
      y = objStateY box
   in box {objStateX = x, objStateY = y}

nextWorld :: Float -> World -> IO World
nextWorld dt world = return world {worldObjects = map (nextBox dt) (worldObjects world)}

-----------------------------------
-- initWorld関連
-----------------------------------
gridLines :: (Int, Int) -> Picture
gridLines (w, h) =
  pictures $
    [color white $ line [(x, fromIntegral $ bottom), (x, fromIntegral $ top)] | x <- map fromIntegral [leftStart, leftStart + objWidth .. right]]
      ++ [color white $ line [(fromIntegral $ left, y), (fromIntegral $ right, y)] | y <- map fromIntegral [bottomStart, bottomStart + objHeight .. top]]
  where
    offsetHeight = if (even h) then objHeight `div` 2 else 0
    offsetWidth = if (even w) then objWidth `div` 2 else 0
    top = (h * objHeight) `div` 2
    bottom = - top
    right = (w * objWidth) `div` 2
    left = - right
    leftStart = left + offsetWidth
    bottomStart = bottom + offsetHeight

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)

initWorld :: IO World
initWorld = do
  let objectToObjKindObjObj :: Object -> ObjKind
      objectToObjKindObjObj a = ObjKindObj a
      objectToObjKindTextText :: Text -> ObjKind
      objectToObjKindTextText a = ObjKindText a

      --obj_images <- loadObjImage OHaskell
      --haskell_images <- loadObjImage THaskell
      --is_images <- loadObjImage TIs
      --you_images <- loadObjImage TYou
      -- obj_images <- mapM loadObjImage (generateEnumValues :: [ObjKind])
      objs = map objectToObjKindObjObj (generateEnumValues :: [Object])
      texts = map objectToObjKindTextText (generateEnumValues :: [Text])
  obj_images <- mapM loadObjImage (objs ++ texts)
  let size = (33, 18)
  let walls = [ObjState x y ObjRight (ObjKindObj OWall) False | x <- [11 .. 21], y <- [6, 10]]
  return $
    updateRule $
      World
        { gridLinePicture = gridLines size,
          imageMap = M.fromList obj_images,
          worldObjects =
            zipWith
              (\g x -> g x)
              ( [ ObjState 11 12 ObjRight (ObjKindText THaskell) True,
                  ObjState 12 12 ObjRight (ObjKindText TIs) True,
                  ObjState 13 12 ObjRight (ObjKindText TYou) True,
                  ObjState 19 12 ObjRight (ObjKindText TFlag) True,
                  ObjState 20 12 ObjRight (ObjKindText TIs) True,
                  ObjState 21 12 ObjRight (ObjKindText TWin) True,
                  ObjState 16 9 ObjRight (ObjKindObj ORock) False,
                  ObjState 16 8 ObjRight (ObjKindObj ORock) False,
                  ObjState 16 7 ObjRight (ObjKindObj ORock) False,
                  ObjState 11 4 ObjRight (ObjKindText TWall) True,
                  ObjState 12 4 ObjRight (ObjKindText TIs) True,
                  ObjState 13 4 ObjRight (ObjKindText TStop) True,
                  ObjState 19 4 ObjRight (ObjKindText TRock) True,
                  ObjState 20 4 ObjRight (ObjKindText TIs) True,
                  ObjState 21 4 ObjRight (ObjKindText TPush) True,
                  ObjState 12 8 ObjRight (ObjKindObj OHaskell) False,
                  ObjState 20 8 ObjLeft (ObjKindObj OFlag) False
                ]
                  ++ walls
              )
              [1 ..],
          worldSize = size,
          rules = []
        }

loadObjImage :: ObjKind -> IO (ObjKind, (PictureLeft, PictureDown, PictureUp, PictureRight))
loadObjImage kind = do
  Just left <- loadPicture kind ObjLeft
  Just down <- loadPicture kind ObjDown
  Just up <- loadPicture kind ObjUp
  Just right <- loadPicture kind ObjRight
  return (kind, (left, down, up, right))

loadPicture :: ObjKind -> Direction -> IO (Maybe Picture)
loadPicture kind dir = loadJuicy ((last $ words $ show kind) ++ "_" ++ (drop 3 $ show dir) ++ ".png")

-----------------------------------
-- main 関数
-----------------------------------

main :: IO ()
main = do
  world <- initWorld
  playIO (window $ worldSize world) black 24 world drawWorld updateWorld nextWorld
