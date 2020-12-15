module Keyboard
    ( FnKey(..),
    ) where

data FnKey = FnReverse | FnStep deriving (Show, Eq)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


