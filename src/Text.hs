module Text
  ( Text (..),
  )
where

data Text = TText | THaskell | TRock | TWall | TFlag | TWin | TStop | TPush | TIs | TYou deriving (Eq, Show, Enum, Ord, Read)
