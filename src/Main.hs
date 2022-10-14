
module Main where

import Graphics.Gloss
import View
import Controller
import Model
import Graphics.Gloss.Interface.IO.Game


{-
windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

main = animate windowDisplay white animationFunc

animationFunc :: Float -> Picture
animationFunc time = Circle (2 * time)


      
-}

type World = (Float, Float)
type Model = (Float, Float)


main :: IO ()
main = play
  windowDisplay
  white
  20
  (0, 0)
  drawingFunc
  inputHandler
  updateFunc

drawingFunc :: Model -> Picture
drawingFunc (theta, dtheta) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]


inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 10)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 10)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 10, y)
inputHandler _ w = w


windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)


updateFunc :: Float -> World -> World
updateFunc _ (x, y) = (towardCenter x, towardCenter y)
  where
    towardCenter :: Float -> Float
    towardCenter c = if abs c < 0.25
      then 0
      else if c > 0
        then c - 0.25
        else c + 0.25
