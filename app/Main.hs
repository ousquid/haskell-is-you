import Action
import Data.Char
import Data.List
import Data.List.Split
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
import System.Environment
import System.Exit
import Text
import Util
import World

-- [ObjKind]
-- - Object
-- - Text
--    * nounList
--    * is
--    * adjectiveList  

-- [Object]
-- - Character
-- - Tile

-----------------------------------
-- updateWorld関連
-----------------------------------

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
  | otherwise = world {worldObjectsList = head (worldObjectsList world) : worldObjectsList world}
duplicateWorldObjects world = world {worldObjectsList = head (worldObjectsList world) : worldObjectsList world}

tailWorldObjects :: World -> World
tailWorldObjects world@(World [x]) = world
tailWorldObjects world = world {worldObjectsList = tail $ worldObjectsList world}

-----------------------------------
-- main 関数
-----------------------------------
window :: Display
window = InWindow "Haskell Is You" (windowWidth, windowHeight) (0, 0)

elapseWorld :: Float -> World -> IO World
elapseWorld dt = return

main :: IO ()
main = do
  let objs = map liftObjKind (generateEnumValues :: [Character])
      texts = map liftObjKind (generateEnumValues :: [Text])
  objImages <- mapM loadObjImage (objs ++ texts)

  args <- getArgs
  let stagePath = case args of
        [] -> "stages/0.csv"
        x : xs -> x
  stage <- words <$> readFile stagePath
  let worldSize = (\(x : y : _) -> (x, y)) $ map read $ splitOn "," $ head stage
  let world = initWorld worldSize $ tail stage
  playIO window black 24 world (drawWorld worldSize $ M.fromList objImages) handleEvent elapseWorld
