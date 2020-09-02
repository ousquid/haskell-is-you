module Main where
import Data.Maybe
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game

-------------------
-- Display の設定
-------------------

windowWidth, windowHeight :: Num a => a
windowWidth = 640
windowHeight = 480

window :: Display
window = InWindow "Hello World" (windowWidth, windowHeight) (100, 100)

--------------------------
-- シミュレーションの実装
--------------------------

boxWidth, boxHeight :: Float
boxWidth  = 50
boxHeight = 50

pickPicture :: (Picture, Picture, Picture, Picture) -> ObjDir -> Picture
pickPicture (x, _, _, _) ObjLeft = x
pickPicture (_, x, _, _) ObjDown = x
pickPicture (_, _, x, _) ObjUp = x
pickPicture (_, _, _, x) ObjRight = x

type WorldHeight = Int
type WorldWidth  = Int
type PictureLeft  = Picture
type PictureDown  = Picture
type PictureUp    = Picture
type PictureRight = Picture

data World = World
  { imageMap :: [(PictureLeft, PictureDown, PictureUp, PictureRight)]
  , worldObjects :: [ObjState]
  , worldSize :: (WorldHeight, WorldWidth)
  }

data ObjDir = ObjLeft | ObjDown | ObjUp | ObjRight deriving (Show, Eq)
data ObjKind = OHaskell deriving (Show, Enum, Eq)

data ObjState = ObjState
  { objStateX  :: Float -- x 座標の位置
  , objStateY  :: Float -- y 座標の位置
  , objStateVx :: Float -- x 方向の速度
  , objStateVy :: Float -- y 方向の速度
  , objStateDir :: ObjDir
  , objStateKind :: ObjKind
  }

drawObj :: ObjState -> Picture -> Picture
drawObj obj picture = translate (objStateX obj) (objStateY obj) $ scale 0.1 0.1 picture

drawWorld :: World -> IO Picture
drawWorld world = do
    let objPictures = [drawObj obj $ pickPicture ((imageMap world)!!(fromEnum $ objStateKind obj)) (objStateDir obj) | obj <- worldObjects world]
    return (pictures objPictures)

-- | イベントを処理する関数。EventKey以外のイベントは無視する
updateWorld :: Event -> World -> IO World
updateWorld (EventKey key ks _ _) world = return $ updateWorldWithKey key ks world
updateWorld (EventMotion _)       world = return world
updateWorld (EventResize _)       world = return world

-- | 上下左右の速度を与える関数
up, down, right, left :: ObjState -> ObjState
up    obj = obj { objStateVy = objStateVy obj + 100, objStateDir = ObjUp }
down  obj = obj { objStateVy = objStateVy obj - 100, objStateDir = ObjDown }
right obj = obj { objStateVx = objStateVx obj + 100, objStateDir = ObjRight }
left  obj = obj { objStateVx = objStateVx obj - 100, objStateDir = ObjLeft }

updateWorldWithKey :: Key -> KeyState -> World -> World
updateWorldWithKey key ks world = world { worldObjects = (map (updateObjWithKey key ks) you) ++ remain }
  where (you, remain) = partition (\x -> objStateKind x == OHaskell) (worldObjects world)

-- | 方向キーとWASDキーに対応して四角形を移動させる
updateObjWithKey :: Key -> KeyState -> ObjState -> ObjState
updateObjWithKey (SpecialKey KeyUp)    ks = if ks == Down then up    else down
updateObjWithKey (SpecialKey KeyDown)  ks = if ks == Down then down  else up
updateObjWithKey (SpecialKey KeyRight) ks = if ks == Down then right else left
updateObjWithKey (SpecialKey KeyLeft)  ks = if ks == Down then left  else right
updateObjWithKey (Char 'w')            ks = if ks == Down then up    else down
updateObjWithKey (Char 's')            ks = if ks == Down then down  else up
updateObjWithKey (Char 'd')            ks = if ks == Down then right else left
updateObjWithKey (Char 'a')            ks = if ks == Down then left  else right
updateObjWithKey _ _ = id

nextBox :: Float -> ObjState -> ObjState
nextBox dt box =
  let -- 速度を考慮した次のステップでの位置を計算
      x  = objStateX box + objStateVx box * dt
      y  = objStateY box + objStateVy box * dt

   in box { objStateX = x, objStateY = y }

nextWorld :: Float -> World -> IO World
nextWorld dt world = return world { worldObjects = map (nextBox dt) (worldObjects world) }

initWorld :: IO World
initWorld = do
  images <- loadObjImage OHaskell
  return World {
    imageMap = [images],
    worldObjects = [ObjState 0 0 0 0 ObjRight OHaskell],
    worldSize = (100, 100)
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

-------------
-- main 関数
-------------

main :: IO ()
main = do
  world <- initWorld
  playIO window black 24 world drawWorld updateWorld nextWorld
