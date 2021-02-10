module Character
  ( Character (..),
  )
where

data Character = CHaskell | CRock | CWall | CFlag | CWater | CVoid deriving (Eq, Show, Enum, Ord, Read)
