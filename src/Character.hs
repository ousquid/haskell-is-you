module Character
  ( Character (..),
  )
where

data Character = CHaskell | CRock | CWall | CFlag | CWater | CSkull | CVoid deriving (Eq, Show, Enum, Ord, Read)
