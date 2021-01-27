module Character
  ( Character (..),
  )
where

data Character = CHaskell | CRock | CWall | CFlag | CVoid deriving (Eq, Show, Enum, Ord, Read)
