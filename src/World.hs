module World
  ( World (..),
    Object (..),
    Character (..),
    WorldWidth,
    WorldHeight,
    Icon (..),
    PictureLeft,
    PictureDown,
    PictureUp,
    PictureRight,
    liftObject,
    worldObjects,
    getRules,
  )
where

import Character (Character (..))
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import qualified Direction as D
import Graphics.Gloss (Picture)
import Rule (Rule (..), adjectiveList, nounList)
import Tile (Tile (..))

type WorldHeight = Int

type WorldWidth = Int

newtype World = World
  { worldObjectsList :: [[Object]]
  }

type PictureLeft = Picture

type PictureDown = Picture

type PictureUp = Picture

type PictureRight = Picture

worldObjects :: World -> [Object]
worldObjects world = head $ worldObjectsList world

defaultRule :: [Rule]
defaultRule =
  [ Rule {ruleS = TText, ruleV = TIs, ruleC = TPush},
    Rule {ruleS = TVoid, ruleV = TIs, ruleC = TStop}
  ]

getRules :: World -> [Rule]
getRules world = filter validRule (concatMap (getRules' tiles) is_list) ++ defaultRule
  where
    validRule rule = ruleS rule `elem` nounList && ruleC rule `elem` (nounList ++ adjectiveList)
    is_list :: [Object]
    is_list = filter (\obj -> objectIcon obj == OTile TIs) (worldObjects world)

    getTile :: Object -> [Object] -- ObjectがTileならそのものを、ObjectがObjectなら空を返す
    getTile obj = case objectIcon obj of
      OTile _ -> [obj]
      OCharacter _ -> []
    tiles = concatMap getTile (worldObjects world)
    getRules' :: [Object] -> Object -> [Rule]
    getRules' tiles is = verticalRule ++ horizontalRule
      where
        x = objectX is
        y = objectY is
        upObj = findTile tiles (x, y + 1)
        downObj = findTile tiles (x, y -1)
        verticalRule = createRule upObj downObj
        leftObj = findTile tiles (x -1, y)
        rightObj = findTile tiles (x + 1, y)
        horizontalRule = createRule leftObj rightObj

        createRule sObj cObj = case (sObj, cObj) of
          (Just a, Just b) -> [Rule {ruleS = a, ruleV = TIs, ruleC = b}]
          _ -> []
        findTile :: [Object] -> (Int, Int) -> Maybe Tile
        findTile objects (x, y) = do
          obj <- findObject objects (x, y)
          let OTile txt = objectIcon obj
          return txt

findObject :: [Object] -> (Int, Int) -> Maybe Object
findObject objects (x, y) = find (\obj -> x == objectX obj && y == objectY obj) objects

data Object = Object
  { objectX :: Int,
    objectY :: Int,
    objectDir :: D.Direction,
    objectIcon :: Icon,
    objectId :: Int
  }
  deriving (Show, Eq)

class ObjectInterface a where
  liftObject :: a -> Icon

instance ObjectInterface Tile where
  liftObject = OTile

instance ObjectInterface Character where
  liftObject = OCharacter

data Icon = OTile Tile | OCharacter Character deriving (Eq, Show, Ord)
