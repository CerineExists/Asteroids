
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


-- The screen which is 1000 by 500
windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 500) (250, 150)


-- Create the initial world
initialWorld = World {player = Player (Location 0 0) (Vector2d 0 1) (Vector2d 0 0),
                      keys   = [],
                      asteroids = asteroidList,
                      bullets = []
                     }


-- The initial asteroidlist
asteroidList :: [Asteroid]
asteroidList =  [   mkAsteroid (Middle (-15) 20) 2 (Vector2d 1 0) (Vector2d 0 0), 
                    mkAsteroid (Middle (-15) 10) 2 (Vector2d 3 0) (Vector2d 0 0), 
                    mkAsteroid (Middle (-15) (-10)) 2 (Vector2d 5 0) (Vector2d 0 0),  
                    mkAsteroid (Middle (-15) (-20)) 2 (Vector2d 0.5 0) (Vector2d 0 0)
                ] 

mkAsteroid :: Middle -> Radius -> Velocity -> Direction -> Asteroid
mkAsteroid = Asteroid 





