
module Main where


import Model
import View
import Controller
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO
  windowDisplay     -- display mode
  black             -- background color
  20                -- number of simulation steps to take for each second of real time !!!!TODO ADD DeltaTime!!!!!
  initialWorld      -- The initial world
  viewBMP           -- convert the world into a picture
  input             -- handle input events
  step              -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.



windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 500) (250, 150)


initialWorld :: World
initialWorld = World {player = Player (Location 0 0) (Vector2d 0 1) (Vector2d 0 0),
                      keys   = []
                     }




