module Draw
  ( loadObjImage,
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
windowWidth = 640
windowHeight = 480

getObjSize :: (Int, Int) -> Int
getObjSize (stageWidth, stageHeight) = min objWidth objHeight
  where
    objWidth = windowWidth `div` stageWidth
    objHeight = windowHeight `div` stageHeight

objImgScale :: (Int, Int) -> Float
objImgScale stageSize = objSizeFloat / 320
  where
    objSizeFloat = fromIntegral $ getObjSize stageSize

loadObjImage :: ObjKind -> IO (ObjKind, (PictureLeft, PictureDown, PictureUp, PictureRight))
loadObjImage kind = do
  left <- loadPicture kind D.Left
  down <- loadPicture kind D.Down
  up <- loadPicture kind D.Up
  right <- loadPicture kind D.Right
  return (kind, (left, down, up, right))

loadPicture :: ObjKind -> D.Direction -> IO Picture
loadPicture kind dir = do
  maybePic <- loadJuicy ("imgs/" ++ last (words $ show kind) ++ "_" ++ show dir ++ ".png")
  return $ case maybePic of
    Just img -> img
    Nothing -> color red $ circleSolid 160

gridLines :: (Int, Int) -> Picture
gridLines (stageWidth, stageHeight) =
  pictures $
    [color white $ line [(x, fromIntegral bottom), (x, fromIntegral top)] | x <- map fromIntegral [leftStart, leftStart + objSize .. right]]
      ++ [color white $ line [(fromIntegral left, y), (fromIntegral right, y)] | y <- map fromIntegral [bottomStart, bottomStart + objSize .. top]]
  where
    offsetHeight = if even stageHeight then objSize `div` 2 else 0
    offsetWidth = if even stageWidth then objSize `div` 2 else 0
    top = (stageHeight * objSize) `div` 2
    bottom = - top
    right = (stageWidth * objSize) `div` 2
    left = - right
    leftStart = left + offsetWidth
    bottomStart = bottom + offsetHeight
    objSize = getObjSize (stageWidth, stageHeight)

drawObj :: (WorldWidth, WorldHeight) -> ObjState -> Picture -> Picture
drawObj (width, height) obj picture = translate (fromIntegral $ (objStateX obj - width `div` 2) * objSize) (fromIntegral $ (objStateY obj - height `div` 2) * objSize) $ scale objScale objScale picture
  where
    objScale = objImgScale (width, height)
    objSize = getObjSize (width, height)

pickPicture :: (Picture, Picture, Picture, Picture) -> D.Direction -> Picture
pickPicture (x, _, _, _) D.Left = x
pickPicture (_, x, _, _) D.Down = x
pickPicture (_, _, x, _) D.Up = x
pickPicture (_, _, _, x) D.Right = x

drawWorld :: (Int, Int) -> M.Map ObjKind (PictureLeft, PictureDown, PictureUp, PictureRight) -> World -> IO Picture
drawWorld worldSize objImages world = do
  let objPictures = [drawObj worldSize obj $ pickPicture (objImages M.! objStateKind obj) (objStateDir obj) | obj <- worldObjects world]
  return (pictures (objPictures ++ [gridLines worldSize]))
