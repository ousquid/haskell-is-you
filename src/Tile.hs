module Tile
  ( Tile (..),
  )
where

data Tile = TVoid | TText | THaskell | TRock | TWall | TFlag | TSkull | TWin | TStop | TPush | TIs | TYou | TWater | TSink | TDefeat deriving (Eq, Show, Enum, Ord, Read)
