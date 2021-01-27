module Util
  ( tile2Character,
    applyHead,
    changeHead,
    generateEnumValues,
  )
where

import Character (Character(..))
import Tile (Tile)

tile2Character :: Tile -> Character
tile2Character tile = read $ "C" ++ drop 1 (show tile)

applyHead :: (a -> a) -> [a] -> [a]
applyHead f (x : xs) = f x : xs

changeHead :: a -> [a] -> [a]
changeHead y (x : xs) = y : xs

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)
