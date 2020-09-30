module Main where
import Data.Maybe
import Data.List
import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game

-------------------
-- Display の設定
-------------------

window :: (Int, Int) -> Display
window (w, h) = InWindow "Hello World" (w*objWidth, h*objHeight) (0, 0)

--------------------------
-- シミュレーションの実装
--------------------------

objWidth, objHeight :: Num a => a
objWidth  = 64
objHeight = 64

objImgScale :: Float
objImgScale  = objWidth / 320

type WorldHeight = Int
type WorldWidth  = Int
type PictureLeft  = Picture
type PictureDown  = Picture
type PictureUp    = Picture
type PictureRight = Picture

data World = World
  { gridLinePicture :: Picture
  , imageMap :: [(PictureLeft, PictureDown, PictureUp, PictureRight)]
  , worldObjects :: [ObjState]
  , worldSize :: (WorldWidth, WorldHeight)
--  , rules :: [Rule]
  }

-- data Rule = Rule
--   { priority :: Int
--   , S :: ?
--   , V :: ?
--   , C :: ?
--   }
-- 1.


-- <BABA AND KEKE> <NOT ON [KEY AND STONE]> IS YOU
--   BABA NOT ON KEY AND STONE IS YOU
--   KEKE NOT ON KEY AND STONE IS YOU

data ObjDir = ObjLeft | ObjDown | ObjUp | ObjRight deriving (Show, Eq)
data ObjKind = OHaskell | THaskell | ORock | TRock | OWall |
 TWall | OFlag | TFlag | TWin | TStop | TPush | TIs | TYou deriving (Show, Enum, Eq)
-- data ObjKind = ObjKindObj ObjObj | ObjKindText ObjText
-- data ObjObj = OHaskell deriving (Show, Enum, Eq)
-- data ObjText = THaskell | TIs | TYou deriving (Show, Enum, Eq)

data ObjState = ObjState
  { objStateX  :: Int
  , objStateY  :: Int
  , objStateDir :: ObjDir
  , objStateKind :: ObjKind
  , objStateIText :: Bool
  }

-----------------------------------
-- drawWorld関連
-----------------------------------

drawObj :: (WorldWidth, WorldHeight) -> ObjState -> Picture -> Picture
drawObj (width, height) obj picture = translate ((fromIntegral $ (objStateX obj) - width `div` 2)*objWidth) ((fromIntegral $ (objStateY obj) - height `div` 2)*objHeight) $ scale objImgScale objImgScale picture

pickPicture :: (Picture, Picture, Picture, Picture) -> ObjDir -> Picture
pickPicture (x, _, _, _) ObjLeft = x
pickPicture (_, x, _, _) ObjDown = x
pickPicture (_, _, x, _) ObjUp = x
pickPicture (_, _, _, x) ObjRight = x

drawWorld :: World -> IO Picture
drawWorld world = do
    let objPictures = [drawObj (worldSize world) obj $ pickPicture ((imageMap world)!!(fromEnum $ objStateKind obj)) (objStateDir obj) | obj <- worldObjects world]
    return (pictures (objPictures ++ [gridLinePicture world]))

-----------------------------------
-- updateWorld関連
-----------------------------------

-- | イベントを処理する関数。EventKey以外のイベントは無視する
updateWorld :: Event -> World -> IO World
updateWorld (EventKey key ks _ _) world = return $ updateWorldWithKey key ks world
updateWorld (EventMotion _)       world = return world
updateWorld (EventResize _)       world = return world

-- | 上下左右の速度を与える関数
up, down, right, left :: ObjState -> ObjState
up    obj = obj { objStateY = objStateY obj + 1, objStateDir = ObjUp }
down  obj = obj { objStateY = objStateY obj - 1, objStateDir = ObjDown }
right obj = obj { objStateX = objStateX obj + 1, objStateDir = ObjRight }
left  obj = obj { objStateX = objStateX obj - 1, objStateDir = ObjLeft }

updateWorldWithKey :: Key -> KeyState -> World -> World
updateWorldWithKey key ks world = world { worldObjects = (map (updateObjWithKey key ks) you) ++ remain }
  where (you, remain) = partition (\x -> objStateKind x == OHaskell) (worldObjects world)

-- | 方向キーとWASDキーに対応して四角形を移動させる
updateObjWithKey :: Key -> KeyState -> ObjState -> ObjState
updateObjWithKey (SpecialKey KeyUp)    Down = up
updateObjWithKey (SpecialKey KeyDown)  Down = down
updateObjWithKey (SpecialKey KeyRight) Down = right
updateObjWithKey (SpecialKey KeyLeft)  Down = left
updateObjWithKey (Char 'w')            Down = up
updateObjWithKey (Char 's')            Down = down
updateObjWithKey (Char 'd')            Down = right
updateObjWithKey (Char 'a')            Down = left
updateObjWithKey _ _ = id

-----------------------------------
-- nextWorld関連
-----------------------------------
nextBox :: Float -> ObjState -> ObjState
nextBox dt box =
  let
      x  = objStateX box
      y  = objStateY box

  in box { objStateX = x, objStateY = y }

nextWorld :: Float -> World -> IO World
nextWorld dt world = return world { worldObjects = map (nextBox dt) (worldObjects world) }

-----------------------------------
-- initWorld関連
-----------------------------------
gridLines :: (Int, Int) -> Picture
gridLines (w, h) = pictures $
  [color white $ line [(x, fromIntegral $ bottom), (x, fromIntegral $ top)] | x <- map fromIntegral [leftStart, leftStart+objWidth..right]] ++
  [color white $ line [(fromIntegral $ left, y), (fromIntegral $ right, y)] | y <- map fromIntegral [bottomStart, bottomStart+objHeight..top]]
  where offsetHeight = if (even h) then objHeight `div` 2 else 0
        offsetWidth = if (even w) then objWidth `div` 2 else 0
        top    = (h * objHeight) `div` 2
        bottom = -top
        right  = (w * objWidth) `div` 2
        left   = -right
        leftStart = left + offsetWidth
        bottomStart = bottom + offsetHeight


initWorld :: IO World
initWorld = do
  let generateEnumValues :: (Enum a) => [a]
      generateEnumValues = enumFrom (toEnum 0)



  --obj_images <- loadObjImage OHaskell
  --haskell_images <- loadObjImage THaskell
  --is_images <- loadObjImage TIs
  --you_images <- loadObjImage TYou
  obj_images <- mapM loadObjImage (generateEnumValues :: [ObjKind])
  let size = (33, 18)
  let walls = [ObjState x y ObjRight OWall False | x <- [11..21], y <- [6, 10]]
  return World {
    gridLinePicture = gridLines size,
    imageMap = obj_images,
    worldObjects = [ObjState 11 12 ObjRight THaskell True,
                    ObjState 12 12 ObjRight TIs True,
                    ObjState 13 12 ObjRight TYou True,
                    ObjState 19 12 ObjRight TFlag True,
                    ObjState 20 12 ObjRight TIs True,
                    ObjState 21 12 ObjRight TWin True,
                    ObjState 16 9 ObjRight ORock False,
                    ObjState 16 8 ObjRight ORock False,
                    ObjState 16 7 ObjRight ORock False,
                    ObjState 11 4 ObjRight TWall True,
                    ObjState 12 4 ObjRight TIs True,
                    ObjState 13 4 ObjRight TStop True,
                    ObjState 19 4 ObjRight TRock True,
                    ObjState 20 4 ObjRight TIs True,
                    ObjState 21 4 ObjRight TPush True,
                    ObjState 12 8 ObjRight OHaskell False,
                    ObjState 20 8 ObjLeft OFlag False] ++ walls,
    worldSize = size
  }

loadObjImage :: ObjKind -> IO (PictureLeft, PictureDown, PictureUp, PictureRight)
loadObjImage kind = do
    Just left <- loadPicture kind ObjLeft
    Just down <- loadPicture kind ObjDown
    Just up <- loadPicture kind ObjUp
    Just right <- loadPicture kind ObjRight
    return (left, down, up, right)

loadPicture :: ObjKind -> ObjDir -> IO (Maybe Picture)
loadPicture kind dir  = loadJuicy ((show kind) ++ "_" ++ (drop 3 $ show dir) ++ ".jpg")

-----------------------------------
-- main 関数
-----------------------------------

main :: IO ()
main = do
  world <- initWorld
  playIO (window $ worldSize world) black 24 world drawWorld updateWorld nextWorld
