module Character
  ( Character (..),
  )
where

data Character = CHaskell | CRock | CWall | CFlag | CWater | CSkull | CLava | CVoid | CKey | CDoor deriving (Eq, Show, Enum, Ord, Read)
