module World
    ( World(..),
      ObjState(..),
      Text(..),
      Object(..),
      WorldWidth,
      WorldHeight,
      Direction(..),
      Rule(..),
      ObjKind(..),
      PictureLeft, PictureDown, PictureUp, PictureRight,
      liftObjState
    ) where

import Graphics.Gloss ( Picture )
import qualified Data.Map.Strict as M
import Data.List ( intercalate )


type WorldHeight = Int
type WorldWidth = Int

data World = World
  { gridLinePicture :: Picture,
    imageMap :: M.Map ObjKind (PictureLeft, PictureDown, PictureUp, PictureRight),
    worldObjectsList :: [[ObjState]],
    worldSize :: (WorldWidth, WorldHeight),
    rules :: [Rule]
  }

type PictureLeft = Picture
type PictureDown = Picture
type PictureUp = Picture
type PictureRight = Picture



data Rule = Rule
  { ruleS :: Text,
    ruleV :: Text,
    ruleC :: Text
  }
  deriving (Eq)
  
data ObjState = ObjState
  { objStateX :: Int,
    objStateY :: Int,
    objStateDir :: Direction,
    objStateKind :: ObjKind,
    objStateIText :: Bool,
    objStateId :: Int
  }
  deriving (Show, Eq)


instance Show Rule where
  show (Rule s v c) = " " ++ (intercalate " " $ map (tail . show) [s, v, c])



class ObjKindInterface a where
  liftObjState :: a -> ObjKind

instance ObjKindInterface Text where
  liftObjState text = ObjKindText text

instance ObjKindInterface Object where
  liftObjState obj = ObjKindObj obj

data ObjKind = ObjKindText Text | ObjKindObj Object deriving (Eq, Show, Ord)
  
data Direction = ObjLeft | ObjDown | ObjUp | ObjRight deriving (Show, Eq)
data Text = TText | THaskell | TRock | TWall | TFlag | TWin | TStop | TPush | TIs | TYou deriving (Eq, Show, Enum, Ord, Read)


data Object = OHaskell | ORock | OWall | OFlag deriving (Eq, Show, Enum, Ord, Read)