module Keyboard
  ( keyToAction,
    Action (..),
  )
where

import qualified Direction as D
import Graphics.Gloss.Interface.IO.Game

data Action = Reverse | Step | Move D.Direction | DoNothing

keyToAction :: Key -> Action
keyToAction (SpecialKey KeyLeft) = Move D.Left
keyToAction (SpecialKey KeyDown) = Move D.Down
keyToAction (SpecialKey KeyUp) = Move D.Up
keyToAction (SpecialKey KeyRight) = Move D.Right
keyToAction (Char 'a') = Move D.Left
keyToAction (Char 's') = Move D.Down
keyToAction (Char 'w') = Move D.Up
keyToAction (Char 'd') = Move D.Right
keyToAction (Char 'f') = Step
keyToAction (Char 'b') = Reverse
keyToAction _ = DoNothing
