module Object
  ( Object (..),
  )
where

data Object = OHaskell | ORock | OWall | OFlag deriving (Eq, Show, Enum, Ord, Read)
