module Rule
  ( Rule (..),
    nounList,
    adjectiveList,
  )
where

import Text (Text (..))

data Rule = Rule
  { ruleS :: Text,
    ruleV :: Text,
    ruleC :: Text
  }
  deriving (Eq)

instance Show Rule where
  show (Rule s v c) = " " ++ unwords (map (tail . show) [s, v, c])

nounList = [THaskell, TRock, TWall, TFlag, TText]

adjectiveList = [TWin, TStop, TPush, TYou]
