
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


-- Het scherm is 
windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 500) (250, 150)


asteroidList :: [Asteroid]
asteroidList =  [   mkAsteroid (-30) 20 2 3 90, 
                    mkAsteroid  (-50) 10 2 2 90,  
                    mkAsteroid (-50) (-10) 2 1 90,   
                    mkAsteroid 30 (-10) 2 3 90
                ] 

mkAsteroid :: Float -> Float -> Float -> Float -> Float -> Asteroid
mkAsteroid x y = Asteroid (Middle x y)


initialWorld = World {player = Player (Location 0 0) 0,
                      keys   = [],
                      asteroids = asteroidList
                     }




