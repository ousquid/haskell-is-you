module Tile
  ( Tile (..),
  )
where

data Tile = TVoid | TText | THaskell | TRock | TWall | TFlag | TWin | TStop | TPush | TIs | TYou deriving (Eq, Show, Enum, Ord, Read)
