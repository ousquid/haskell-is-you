module World
  ( World (..),
    ObjState (..),
    Character (..),
    WorldWidth,
    WorldHeight,
    ObjKind (..),
    PictureLeft,
    PictureDown,
    PictureUp,
    PictureRight,
    liftObjKind,
    worldObjects,
    getRules,
  )
where

import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import qualified Direction as D
import Graphics.Gloss (Picture)
import Character (Character (..))
import Rule (Rule (..), adjectiveList, nounList)
import Tile (Tile (..))

type WorldHeight = Int

type WorldWidth = Int

newtype World = World
  { worldObjectsList :: [[ObjState]]
  }

type PictureLeft = Picture

type PictureDown = Picture

type PictureUp = Picture

type PictureRight = Picture

worldObjects :: World -> [ObjState]
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
    is_list :: [ObjState]
    is_list = filter (\obj -> objStateKind obj == ObjKindTile TIs) (worldObjects world)

    getTile :: ObjState -> [ObjState] -- ObjStateがTileならそのものを、ObjStateがObjectなら空を返す
    getTile obj = case objStateKind obj of
      ObjKindTile _ -> [obj]
      ObjKindObj _ -> []
    tiles = concatMap getTile (worldObjects world)
    getRules' :: [ObjState] -> ObjState -> [Rule]
    getRules' tiles is = verticalRule ++ horizontalRule
      where
        x = objStateX is
        y = objStateY is
        upObj = findTile tiles (x, y + 1)
        downObj = findTile tiles (x, y -1)
        verticalRule = createRule upObj downObj
        leftObj = findTile tiles (x -1, y)
        rightObj = findTile tiles (x + 1, y)
        horizontalRule = createRule leftObj rightObj

        createRule sObj cObj = case (sObj, cObj) of
          (Just a, Just b) -> [Rule {ruleS = a, ruleV = TIs, ruleC = b}]
          _ -> []
        findTile :: [ObjState] -> (Int, Int) -> Maybe Tile
        findTile objects (x, y) = do
          obj <- findObject objects (x, y)
          let ObjKindTile txt = objStateKind obj
          return txt

findObject :: [ObjState] -> (Int, Int) -> Maybe ObjState
findObject objects (x, y) = find (\obj -> x == objStateX obj && y == objStateY obj) objects

data ObjState = ObjState
  { objStateX :: Int,
    objStateY :: Int,
    objStateDir :: D.Direction,
    objStateKind :: ObjKind,
    objStateITile :: Bool,
    objStateId :: Int
  }
  deriving (Show, Eq)

class ObjKindInterface a where
  liftObjKind :: a -> ObjKind

instance ObjKindInterface Tile where
  liftObjKind = ObjKindTile

instance ObjKindInterface Character where
  liftObjKind = ObjKindObj 

data ObjKind = ObjKindTile Tile | ObjKindObj Character deriving (Eq, Show, Ord)
