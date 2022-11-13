
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
    ufo <- loadBMP "ufoBMP.bmp"
    space <- loadBMP "space.bmp"        -- https://opengameart.org/content/space-backdrop
    asteroid <- loadBMP "asteroid.bmp"  -- https://opengameart.org/content/asteroid-generator-and-a-set-of-generated-asteroids
    seed <- initStdGen
    playIO
        windowDisplay     -- display mode
        black             -- background color
        20                -- number of simulation steps to take for each second of real time !!!!TODO ADD DeltaTime!!!!!
        (initialWorld raket [rRocket1, rRocket2, rRocket3, rRocket4] space asteroid ufo seed)   -- The initial world
        viewBMP           -- convert the world into a picture
        input             -- handle input events
        step              -- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


-- The screen which is 1000 by 500
windowDisplay :: Display
windowDisplay = InWindow "Window" (1000, 500) (250, 150)


-- Create the initial world
initialWorld :: Picture -> [Picture] -> Picture -> Picture -> Picture-> StdGen -> World
initialWorld raket runningRockets space asteroid ufo seed = 
        World {player = Player (Location 0 0) (Vector2d 0 1) (Vector2d 0 0),
                            keys   = [],
                            asteroids = asteroidList,
                            bullets = [],
                            state = Playing,
                            score = 0,
                            pics = PicList raket runningRockets space asteroid ufo, 
                            seed = seed,
                            enemies = ufoList,
                            activeUFO = 0,
                            elapsedTime = 0
}

-- Create the UFO's that will later appear. Status starts at Waiting, will become Attacking at one point and may become Killed.
ufoList :: [UFO] --         locationUFO              velocityUFO     speedUFO  size  bulletsUFO    stateUFO lastShotAt  number
ufoList =       [   UFO (Location (-500) (-250))    (Vector2d 5 5)      5        3      []          Waiting   0          0,
                    UFO (Location    500 (-100))    (Vector2d 2 5)      5        6      []          Waiting     0          1,
                    UFO (Location (-200)   250)    (Vector2d 5 3)      5        3      []          Waiting     0          2]

-- The initial asteroidlist
asteroidList :: [Asteroid]
asteroidList =  [   Asteroid (Middle (-500) 20) 10 (Vector2d 3 2) 3, 
                    Asteroid (Middle (-500) 100) 20 (Vector2d 4 3) 4, 
                    Asteroid (Middle (-100) (-250)) 40 (Vector2d 5 6) 5,  
                    Asteroid (Middle 30 250) 40 (Vector2d 6 7) 6
                ] 







