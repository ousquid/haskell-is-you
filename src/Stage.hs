module Stage
  ( initWorld,
  )
where

import qualified Data.Map as M
import qualified Direction as D
import Draw
import Graphics.Gloss
import World

initWorld :: [(ObjKind, (PictureLeft, PictureDown, PictureUp, PictureRight))] -> World
initWorld obj_images =
  let stageSize = (33, 18)
      walls = [ObjState x y D.Right (ObjKindObj OWall) False | x <- [11 .. 21], y <- [6, 10]]
   in World
        { gridLinePicture = gridLines stageSize,
          imageMap = M.fromList obj_images,
          worldObjectsList =
            [ zipWith
                (\g x -> g x)
                ( [ ObjState 11 12 D.Right (ObjKindText THaskell) True,
                    ObjState 12 12 D.Right (ObjKindText TIs) True,
                    ObjState 13 12 D.Right (ObjKindText TYou) True,
                    ObjState 19 12 D.Right (ObjKindText TFlag) True,
                    ObjState 20 12 D.Right (ObjKindText TIs) True,
                    ObjState 21 12 D.Right (ObjKindText TWin) True,
                    ObjState 16 9 D.Right (ObjKindObj ORock) False,
                    ObjState 16 8 D.Right (ObjKindObj ORock) False,
                    ObjState 16 7 D.Right (ObjKindObj ORock) False,
                    ObjState 11 4 D.Right (ObjKindText TWall) True,
                    ObjState 12 4 D.Right (ObjKindText TIs) True,
                    ObjState 13 4 D.Right (ObjKindText TStop) True,
                    ObjState 19 4 D.Right (ObjKindText TRock) True,
                    ObjState 20 4 D.Right (ObjKindText TIs) True,
                    ObjState 21 4 D.Right (ObjKindText TPush) True,
                    ObjState 12 8 D.Right (ObjKindObj OHaskell) False,
                    ObjState 20 8 D.Left (ObjKindObj OFlag) False
                  ]
                    ++ walls
                )
                [1 ..]
            ],
          worldSize = stageSize
        }
