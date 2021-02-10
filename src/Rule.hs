module Rule
  ( Rule (..),
    nounList,
    adjectiveList,
  )
where

import Tile (Tile (..))

data Rule = Rule
  { ruleS :: Tile,
    ruleV :: Tile,
    ruleC :: Tile
  }
  deriving (Eq)

instance Show Rule where
  show (Rule s v c) = " " ++ unwords (map (tail . show) [s, v, c])

nounList = [THaskell, TRock, TWall, TFlag, TText, TWater]

adjectiveList = [TWin, TStop, TSink, TPush, TYou]
