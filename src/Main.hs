
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

data World = World Player 
type Model = (Float, Float)


main :: IO ()
main = playIO
  windowDisplay     -- display mode
  black             -- background color
  20                -- number of simulation steps to take for each second of real time
  initialWorld      -- The initial world
  view              -- convert the world into a picture
  input             -- handle input events
  step              -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.



windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 500) (250, 150)


initialWorld :: World
initialWorld = (World (Player (Location 0 0) (Direction 0 0) ))


-- | Tekenen
view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture --teken ruimteschip IS NU NOG EEN CIRKEL
viewPure (World (Player (Location x y) direction)) = pictures [space,  translate (x*10) (y*10) raket]
        where
            raket = color white (thickCircle 3 30)
            space = color roze $ polygon (rectanglePath 1000 500) 
            roze = makeColor 0.9648 0.1055 0.7929 0.5  


{-

||| dit werkt maar kan never nooit bewegen
view :: World -> IO Picture
view _ = loadBMP "raketBMP.bmp"
|||

view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture --teken ruimteschip IS NU NOG EEN CIRKEL
viewPure _ = pictures [space, raket]
        where
            raket = (>>=) $ loadBMP "raketBMP.bmp"
            space = (>>=) $ loadBMP "space.bmp"

-}



-- | Handle user input
input :: Event -> World -> IO World
input e wrld = return (inputKey e wrld)

inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeyUp) Down _ _)    (World (Player (Location x y) direction ))  = World (Player (Location x (y + 1)) direction)
inputKey (EventKey (SpecialKey KeyDown) Down _ _)  (World (Player (Location x y) direction ))  = World (Player (Location x (y - 1)) direction)
inputKey (EventKey (SpecialKey KeyRight) Down _ _) (World (Player (Location x y) direction ))  = World (Player (Location (x + 1) y) direction)
inputKey (EventKey (SpecialKey KeyLeft) Down _ _)  (World (Player (Location x y) direction ))  = World (Player (Location (x - 1) y) direction)
inputKey _ w = w



step :: Float -> World -> IO World
step _ wrld = return wrld


