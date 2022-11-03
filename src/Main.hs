
module Main where


import Model
import View
import Controller
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


main :: IO ()
main = do
    raket <- loadBMP "rocketStill.bmp"     -- self made
    rRocket1 <- loadBMP "runningRocket1.bmp"
    rRocket2 <- loadBMP "runningRocket2.bmp"
    rRocket3 <- loadBMP "runningRocket3.bmp"
    rRocket4 <- loadBMP "runningRocket4.bmp"
    space <- loadBMP "space.bmp"        -- https://opengameart.org/content/space-backdrop
    asteroid <- loadBMP "asteroid.bmp"  -- https://opengameart.org/content/asteroid-generator-and-a-set-of-generated-asteroids
    seed <- initStdGen
    playIO
        windowDisplay     -- display mode
        black             -- background color
        20                -- number of simulation steps to take for each second of real time !!!!TODO ADD DeltaTime!!!!!
        (initialWorld raket [rRocket1, rRocket2, rRocket3, rRocket4] space asteroid seed)   -- The initial world
        viewBMP           -- convert the world into a picture
        input             -- handle input events
        step              -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


-- The screen which is 1000 by 500
windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 500) (250, 150)


-- Create the initial world
initialWorld :: Picture -> [Picture] -> Picture -> Picture  -> StdGen -> World
initialWorld raket runningRockets space asteroid seed = 
        World {player = Player (Location 0 0) (Vector2d 0 1) (Vector2d 0 0),
                            keys   = [],
                            asteroids = asteroidList,
                            bullets = [],
                            state = Playing,
                            score = 0,
                            pics = PicList raket runningRockets space asteroid, 
                            seed = seed,
                            elapsedTime = 0,
                            lastAsteroidSpawned = 0
}


-- The initial asteroidlist
asteroidList :: [Asteroid]
asteroidList =  [   mkAsteroid (Middle (-15) 20) 10 (Vector2d 5 0) (Vector2d 0 0), 
                    mkAsteroid (Middle (-15) 10) 20 (Vector2d 6 0) (Vector2d 0 0), 
                    mkAsteroid (Middle (-15) (-10)) 40 (Vector2d 7 0) (Vector2d 0 0),  
                    mkAsteroid (Middle 30 100) 40 (Vector2d 10 0) (Vector2d 0 0)
                ] 

mkAsteroid :: Middle -> Radius -> Velocity -> Direction -> Asteroid
mkAsteroid = Asteroid 





