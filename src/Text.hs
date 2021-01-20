module Text
  ( Text (..),
  )
where

data Text = TVoid | TText | THaskell | TRock | TWall | TFlag | TWin | TStop | TPush | TIs | TYou deriving (Eq, Show, Enum, Ord, Read)
