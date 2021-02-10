module Tile
  ( Tile (..),
  )
where

data Tile = TVoid | TText | THaskell | TRock | TWall | TFlag | TWin | TStop | TPush | TIs | TYou | TWater | TSink deriving (Eq, Show, Enum, Ord, Read)
