module Stage
  ( initWorld,
  )
where

import Data.List.Split
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Direction as D
import Draw
import Graphics.Gloss
import System.FilePath.Posix
import Tile
import World

initWorld :: (WorldWidth, WorldHeight) -> String -> [Integer] -> [T.Text] -> World
initWorld (width, height) stage tileIds tileImgPaths = World {worldObjectsList = [zipWith (\g x -> g x) objects [1 ..] ++ voidObjects (width, height)]}
  where
    stageInt = (map read $ splitOn "," stage) :: [Integer]
    idWithCoord = [((idx `mod` width, height - 1 - idx `div` width), id) | (idx, id) <- zip [0 ..] stageInt, id /= 0]
    idToObj = M.fromList $ zip tileIds $ map parseImagePath tileImgPaths
    objects = [Object w h dir kind | ((w, h), id) <- idWithCoord, let (kind, dir) = idToObj M.! id]

parseImagePath :: T.Text -> (Icon, D.Direction)
parseImagePath str = (icon, dir)
  where
    [iconStr, directionStr] = splitOn "_" $ takeBaseName $ T.unpack str
    icon = if head iconStr == 'T' then OTile (read iconStr :: Tile) else OCharacter (read iconStr :: Character)
    dir = read directionStr :: D.Direction

voidObjects :: (WorldWidth, WorldHeight) -> [Object]
voidObjects (width, height) = leftAndRight ++ aboveAndBottom
  where
    leftAndRight = [Object x y D.Down (OCharacter CVoid) 0 | x <- [-1, width], y <- [0 .. height -1]]
    aboveAndBottom = [Object x y D.Down (OCharacter CVoid) 0 | x <- [0 .. width -1], y <- [-1, height]]

stringToObject :: [String] -> (Int -> Object)
stringToObject [x, y, dir, kind] = Object _x _y _dir objKind
  where
    isTile = head kind == 'T'
    _x = read x :: Int
    _y = read y :: Int
    _dir = read dir :: D.Direction
    objKind = if isTile then OTile (read kind :: Tile) else OCharacter (read kind :: Character)
