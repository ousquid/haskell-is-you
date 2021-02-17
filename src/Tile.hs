module Tile
  ( Tile (..),
  )
where

data Tile = TVoid | TText | THaskell | TRock | TWall | TFlag | TSkull | TLava | TWin | TStop | TPush | TIs | TYou | TWater | TSink | TDefeat | THot | TMelt deriving (Eq, Show, Enum, Ord, Read)
