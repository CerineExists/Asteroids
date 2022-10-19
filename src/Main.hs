
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
  20                -- number of simulation steps to take for each second of real time
  initialWorld      -- The initial world
  viewBMP           -- convert the world into a picture
  input             -- handle input events
  step              -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.



windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 500) (250, 150)


initialWorld :: World
initialWorld = World (Player (Location 0 0) 90 ) asteroidList

asteroidList :: [Asteroid]
asteroidList =  [   mkAsteroid (-100) 100 2, 
                    mkAsteroid  500 (-100) 2, 
                    mkAsteroid (-500) (-100) 2,  
                    mkAsteroid 1000 100 2
                ] 

mkAsteroid :: Float -> Float -> Float -> Asteroid
mkAsteroid x y = Asteroid (Middle x y)






