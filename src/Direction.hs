module Direction
  ( Direction (..),
  )
where

data Direction = Left | Down | Up | Right deriving (Show, Read, Eq)
