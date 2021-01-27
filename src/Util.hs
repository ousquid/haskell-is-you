module Util
  ( text2Character,
    applyHead,
    changeHead,
    generateEnumValues,
  )
where

import Character (Character(..))
import Text (Text)

text2Character :: Text -> Character
text2Character text = read $ "C" ++ drop 1 (show text)

applyHead :: (a -> a) -> [a] -> [a]
applyHead f (x : xs) = f x : xs

changeHead :: a -> [a] -> [a]
changeHead y (x : xs) = y : xs

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)
