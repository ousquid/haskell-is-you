module Main where
import Data.Maybe
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

data ObjDir = ObjUp | ObjDown | ObjRight | ObjLeft

data ObjState = ObjState
  { objStateX  :: Float -- x 座標の位置
  , objStateY  :: Float -- y 座標の位置
  , objStateVx :: Float -- x 方向の速度
  , objStateVy :: Float -- y 方向の速度
  , objStateDir :: ObjDir
  }


initialBox :: ObjState
initialBox = ObjState 0 0 0 0 ObjRight

loadPicture :: ObjDir -> IO (Maybe Picture)
loadPicture ObjUp    = loadJuicy "haskell_up.jpg"
loadPicture ObjDown  = loadJuicy "haskell_down.jpg"
loadPicture ObjRight = loadJuicy "haskell_right.jpg"
loadPicture ObjLeft  = loadJuicy "haskell_left.jpg"

drawBox :: ObjState -> IO Picture
drawBox box = do
    Just img <- loadPicture (objStateDir box)
    return (translate (objStateX box) (objStateY box) $ scale 0.1 0.1 img)

-- | イベントを処理する関数。EventKey以外のイベントは無視する
updateBox :: Event -> ObjState -> IO ObjState
updateBox (EventKey key ks _ _) box = return $ updateBoxWithKey key ks box
updateBox (EventMotion _)       box = return box
updateBox (EventResize _)       box = return box

-- | 上下左右の速度を与える関数
up, down, right, left :: ObjState -> ObjState
up    box = box { objStateVy = objStateVy box + 100, objStateDir = ObjUp }
down  box = box { objStateVy = objStateVy box - 100, objStateDir = ObjDown }
right box = box { objStateVx = objStateVx box + 100, objStateDir = ObjRight }
left  box = box { objStateVx = objStateVx box - 100, objStateDir = ObjLeft }

-- | 方向キーとWASDキーに対応して四角形を移動させる
updateBoxWithKey :: Key -> KeyState -> ObjState -> ObjState
updateBoxWithKey (SpecialKey KeyUp)    ks = if ks == Down then up    else down
updateBoxWithKey (SpecialKey KeyDown)  ks = if ks == Down then down  else up
updateBoxWithKey (SpecialKey KeyRight) ks = if ks == Down then right else left
updateBoxWithKey (SpecialKey KeyLeft)  ks = if ks == Down then left  else right
updateBoxWithKey (Char 'w')            ks = if ks == Down then up    else down
updateBoxWithKey (Char 's')            ks = if ks == Down then down  else up
updateBoxWithKey (Char 'd')            ks = if ks == Down then right else left
updateBoxWithKey (Char 'a')            ks = if ks == Down then left  else right
updateBoxWithKey _ _ = id

nextBox :: Float -> ObjState -> IO ObjState
nextBox dt box =
  let -- 速度を考慮した次のステップでの位置を計算
      x  = objStateX box + objStateVx box * dt
      y  = objStateY box + objStateVy box * dt

   in return box { objStateX = x, objStateY = y }

-------------
-- main 関数
-------------

main :: IO ()
main = playIO window black 24 initialBox drawBox updateBox nextBox
