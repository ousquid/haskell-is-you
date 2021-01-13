module World
  ( World (..),
    ObjState (..),
    Text (..),
    Object (..),
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
import Object (Object (..))
import Rule (Rule (..), adjectiveList, nounList)
import Text (Text (..))

type WorldHeight = Int

type WorldWidth = Int

data World = World
  { worldObjectsList :: [[ObjState]]
  }

type PictureLeft = Picture

type PictureDown = Picture

type PictureUp = Picture

type PictureRight = Picture

worldObjects :: World -> [ObjState]
worldObjects world = head $ worldObjectsList world

defaultRule :: [Rule]
defaultRule = [Rule {ruleS = TText, ruleV = TIs, ruleC = TPush}]

getRules :: World -> [Rule]
getRules world = filter validRule (concatMap (getRules' texts) is_list) ++ defaultRule
  where
    validRule rule = ruleS rule `elem` nounList && ruleC rule `elem` (nounList ++ adjectiveList)
    is_list :: [ObjState]
    is_list = filter (\obj -> (objStateKind obj) == (ObjKindText TIs)) (worldObjects world)

    getText :: ObjState -> [ObjState] -- ObjStateがTextならそのものを、ObjStateがObjectなら空を返す
    getText obj = case (objStateKind obj) of
      ObjKindText _ -> [obj]
      ObjKindObj _ -> []
    texts = concatMap getText (worldObjects world)
    getRules' :: [ObjState] -> ObjState -> [Rule]
    getRules' texts is = verticalRule ++ horizontalRule
      where
        x = objStateX is
        y = objStateY is
        upObj = findText texts (x, y + 1)
        downObj = findText texts (x, y -1)
        verticalRule = createRule upObj downObj
        leftObj = findText texts (x -1, y)
        rightObj = findText texts (x + 1, y)
        horizontalRule = createRule leftObj rightObj

        createRule sObj cObj = case (sObj, cObj) of
          (Just a, Just b) -> [Rule {ruleS = a, ruleV = TIs, ruleC = b}]
          otherwise -> []
        findText :: [ObjState] -> (Int, Int) -> Maybe Text
        findText objects (x, y) = do
          obj <- findObject objects (x, y)
          let ObjKindText txt = objStateKind obj
          return txt

findObject :: [ObjState] -> (Int, Int) -> Maybe ObjState
findObject objects (x, y) = find (\obj -> x == (objStateX obj) && y == (objStateY obj)) objects

data ObjState = ObjState
  { objStateX :: Int,
    objStateY :: Int,
    objStateDir :: D.Direction,
    objStateKind :: ObjKind,
    objStateIText :: Bool,
    objStateId :: Int
  }
  deriving (Show, Eq)

class ObjKindInterface a where
  liftObjKind :: a -> ObjKind

instance ObjKindInterface Text where
  liftObjKind text = ObjKindText text

instance ObjKindInterface Object where
  liftObjKind obj = ObjKindObj obj

data ObjKind = ObjKindText Text | ObjKindObj Object deriving (Eq, Show, Ord)
