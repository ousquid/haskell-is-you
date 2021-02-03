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
getRules world = filter validRule (concatMap (getRules' tileObjects) isList) ++ defaultRule
  where
    validRule rule = ruleS rule `elem` nounList && ruleC rule `elem` (nounList ++ adjectiveList)
    isList :: [Object]
    isList = filter (\obj -> objectIcon obj == OTile TIs) (worldObjects world)

    tileObjects = filter isTile (worldObjects world)
    getRules' :: [Object] -> Object -> [Rule]
    getRules' tileObjects is = verticalRule ++ horizontalRule
      where
        x = objectX is
        y = objectY is
        upTile = findTile tileObjects (x, y + 1)
        downTile = findTile tileObjects (x, y -1)
        verticalRule = createRule upTile downTile
        leftTile = findTile tileObjects (x -1, y)
        rightTile = findTile tileObjects (x + 1, y)
        horizontalRule = createRule leftTile rightTile

        createRule sTile cTile = case (sTile, cTile) of
          (Just a, Just b) -> [Rule {ruleS = a, ruleV = TIs, ruleC = b}]
          _ -> []
        findTile :: [Object] -> (Int, Int) -> Maybe Tile
        findTile objects (x, y) = do
          obj <- findObject objects (x, y)
          let OTile txt = objectIcon obj
          return txt

findObject :: [Object] -> (Int, Int) -> Maybe Object
findObject objects (x, y) = find (\obj -> x == objectX obj && y == objectY obj) objects

isTile :: Object -> Bool
isTile obj = case objectIcon obj of
  OTile _ -> True
  OCharacter _ -> False

data Object = Object
  { objectX :: Int,
    objectY :: Int,
    objectDir :: D.Direction,
    objectIcon :: Icon,
    objectId :: Int
  }
  deriving (Show, Eq)

data Icon = OTile Tile | OCharacter Character deriving (Eq, Show, Ord)
