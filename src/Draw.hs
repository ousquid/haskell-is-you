module Draw
  ( loadIconImage,
    gridLines,
    windowWidth,
    windowHeight,
    drawWorld,
  )
where

import qualified Data.Map as M
import qualified Direction as D
import Graphics.Gloss
import Graphics.Gloss.Juicy
import World

windowWidth, windowHeight :: Num a => a
windowWidth = 640 * 2
windowHeight = 480 * 2

getIconSize :: (Int, Int) -> Int
getIconSize (stageWidth, stageHeight) = min iconWidth iconHeight
  where
    iconWidth = windowWidth `div` stageWidth
    iconHeight = windowHeight `div` stageHeight

iconImgScale :: (Int, Int) -> Float
iconImgScale stageSize = iconSizeFloat / 320
  where
    iconSizeFloat = fromIntegral $ getIconSize stageSize

loadIconImage :: Icon -> IO (Icon, (PictureLeft, PictureDown, PictureUp, PictureRight))
loadIconImage kind = do
  left <- loadPicture kind D.Left
  down <- loadPicture kind D.Down
  up <- loadPicture kind D.Up
  right <- loadPicture kind D.Right
  return (kind, (left, down, up, right))

loadPicture :: Icon -> D.Direction -> IO Picture
loadPicture kind dir = do
  maybePic <- loadJuicy ("imgs/" ++ last (words $ show kind) ++ "_" ++ show dir ++ ".png")
  return $ case maybePic of
    Just img -> img
    Nothing -> color red $ circleSolid 160

gridLines :: (Int, Int) -> Picture
gridLines (stageWidth, stageHeight) =
  pictures $
    [color white $ line [(x, fromIntegral bottom), (x, fromIntegral top)] | x <- map fromIntegral [leftStart, leftStart + iconSize .. right]]
      ++ [color white $ line [(fromIntegral left, y), (fromIntegral right, y)] | y <- map fromIntegral [bottomStart, bottomStart + iconSize .. top]]
  where
    offsetHeight = if even stageHeight then iconSize `div` 2 else 0
    offsetWidth = if even stageWidth then iconSize `div` 2 else 0
    top = (stageHeight * iconSize) `div` 2
    bottom = - top
    right = (stageWidth * iconSize) `div` 2
    left = - right
    leftStart = left + offsetWidth
    bottomStart = bottom + offsetHeight
    iconSize = getIconSize (stageWidth, stageHeight)

drawObject :: (WorldWidth, WorldHeight) -> Object -> Picture -> Picture
drawObject (width, height) obj picture = translate x y $ scale iconScale iconScale picture
  where
    iconScale = iconImgScale (width, height)
    iconSize = getIconSize (width, height)
    --(x, y) = map (\(a, b) -> fromIntegral $ (a - b `div` 2) * iconSize) [(objectX obj, width),  (objectY obj, height)]
    x = fromIntegral $ (objectX obj - width `div` 2) * iconSize
    y = fromIntegral $ (objectY obj - height `div` 2) * iconSize

pickPicture :: (Picture, Picture, Picture, Picture) -> D.Direction -> Picture
pickPicture (x, _, _, _) D.Left = x
pickPicture (_, x, _, _) D.Down = x
pickPicture (_, _, x, _) D.Up = x
pickPicture (_, _, _, x) D.Right = x

drawWorld :: (Int, Int) -> M.Map Icon (PictureLeft, PictureDown, PictureUp, PictureRight) -> World -> IO Picture
drawWorld worldSize objImages world = do
  let objPictures = [drawObject worldSize obj $ pickPicture (objImages M.! objectIcon obj) (objectDir obj) | obj <- worldObjects world]
  return (pictures (objPictures ++ [gridLines worldSize]))
