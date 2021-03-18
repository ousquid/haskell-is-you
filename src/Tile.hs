module Tile
  ( Tile (..),
  )
where

data Tile = TVoid | TText | THaskell | TRock | TWall | TFlag | TSkull | TLava | TKey | TDoor | TWin | TStop | TPush | TIs | TYou | TWater | TSink | TDefeat | THot | TMelt | TOpen | TShut deriving (Eq, Show, Enum, Ord, Read)
