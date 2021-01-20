module Object
  ( Object (..),
  )
where

data Object = OHaskell | ORock | OWall | OFlag | OVoid deriving (Eq, Show, Enum, Ord, Read)
