module Util
  ( text2Object,
    applyHead,
    changeHead,
    generateEnumValues,
  )
where

import Object (Object)
import Text (Text)

text2Object :: Text -> Object
text2Object text = read $ "O" ++ drop 1 (show text)

applyHead :: (a -> a) -> [a] -> [a]
applyHead f (x : xs) = f x : xs

changeHead :: a -> [a] -> [a]
changeHead y (x : xs) = y : xs

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)
