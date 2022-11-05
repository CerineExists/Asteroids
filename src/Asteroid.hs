

module Asteroid where

import Model
import HelpFunctions

import Data.Foldable (Foldable(foldr'))
import System.Random
import Data.Maybe
import Data.List






-- | Adjust the asteroids only that are NOT HIT 
adjustAsteroidLocations :: [Asteroid] -> [Asteroid]
adjustAsteroidLocations = map updateAsteroid

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a@(Asteroid (Middle x y) radius v@(Vector2d vx vy) direction)=
      Asteroid (Middle newX newY) radius v direction
      where
                    newX    | x > 510 = -500 
                            | x < -510 = 510
                            | otherwise = x + vx * (mag/10)
                    newY    | y > 255 = -250
                            | y < -255 = 255
                            | otherwise = y + vy * (mag/10)
                    mag = sqrt (vx * vx + vy * vy)







-- | Functions below are for asteroid that were hit

splitAsteroids :: StdGen -> [Asteroid] -> (StdGen, [Asteroid])
splitAsteroids seed = splitAsteroids' seed []
  where
    splitAsteroids' seed acc [] = (seed, acc)
    splitAsteroids' seed acc (x:xs) = let
      (newSeed, asteroids) = splitAsteroid seed x
      in splitAsteroids' newSeed (asteroids++acc) xs


splitAsteroid :: StdGen -> Asteroid -> (StdGen, [Asteroid])
splitAsteroid seed a@(Asteroid m radius (Vector2d vx vy) direction)
                                        = (nextG2,[asteroid1, asteroid2]) 
                                            where
                                                asteroid1 = Asteroid m (radius / 2) (Vector2d newVX1 newVY1) direction
                                                asteroid2 = Asteroid m (radius / 2) (Vector2d newVX2 newVY2) direction

                                                newVX1 = vx * x_Dir1
                                                newVY1 = vy * y_Dir1

                                                newVX2 = vx * x_Dir2
                                                newVY2 = vy * y_Dir2

                                                (x_Dir2, y_Dir2)    = degreeToVector degree2
                                                (degree2, nextG2)   = randomR(0,360) nextG1
                                                (x_Dir1, y_Dir1)    = degreeToVector degree1
                                                (degree1, nextG1)   = randomR (0, 360) seed



-- | new asteroids spawnen
-- Spawns a new asteroid every 3 seconds
-- we kunnen evt de check (time - lastAs) > 3 verplaatsen naar een andere plek en deze functie áltijd gebruiken 
-- voor het spawnen van een nieuwe asteroid
{-
spawnNewAsteroid :: World -> World
spawnNewAsteroid w@World{asteroids = as, seed = s, elapsedTime = time, lastAsteroidSpawned = lastAs}  
                                                        | (time - lastAs) > 3 = w{seed = nextG3, asteroids = newAsteroid : as, lastAsteroidSpawned = time} 
                                                        | otherwise = w
                                            where
                                              newAsteroid :: Asteroid
                                              newAsteroid    = Asteroid findMiddle 20 velocity (Vector2d 0 0) -- (findVelocity locationAsteroid)

                                              findMiddle :: Middle 
                                              findMiddle    | locAsteroid == North  = Middle middleXY 250    -- spawn at top of screen
                                                            | locAsteroid == South  = Middle middleXY (-250)  -- spawn at bottom of screen
                                                            | locAsteroid == East   = Middle 500 middleXY  -- spawn at left of screen                                                          
                                                            | otherwise             = Middle (-500) middleXY   -- spawn at right of screen


                                              -- determine if the asteroid will spawn top, right, bottom or left
                                              locAsteroid :: LocationNewAsteroid
                                              locAsteroid | generator == 0 = North
                                                          | generator == 1 = East
                                                          | generator == 2 = South
                                                          | otherwise = West
                                                              where
                                                                generator = round (time*100) `mod` 4 

                                              -- find the random middlepoint
                                              (middleXY, nextG3)  | locAsteroid == North || locAsteroid == South = randomR (-500, 50) nextG2
                                                                  | otherwise = randomR (-250, 250) nextG2


                                              -- determine velocity
                                              velocity = Vector2d (speed * x_Dir) (speed*y_Dir)
                                              (speed, nextG2) = randomR (5,10) nextG1

                                              (x_Dir, y_Dir) = degreeToVector degree
                                              (degree, nextG1)     = randomR (0, 360) s
                                               
-}
