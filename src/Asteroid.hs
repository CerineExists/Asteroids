

module Asteroid where

import Model
import HelpFunctions
import Bullet
import Data.Foldable (Foldable(foldr'))
import System.Random



-- | Adjust every asteroid in the list
adjustAsteroidList :: World -> [Asteroid] -> [Asteroid] -- return the new list of asteroids
adjustAsteroidList _ [] = []
adjustAsteroidList world (x:xs) = flyingAsteroids world x ++ adjustAsteroidList world xs

-- | Check per asteroid what it's new location is
flyingAsteroids :: World -> Asteroid -> [Asteroid]
flyingAsteroids w@(World (Player location _ _) _ _ bs _ _ _ seed _ _) a@(Asteroid (Middle x y) radius v@(Vector2d vx vy) direction)
            -- | x < -500  || x > 500 || y < -250 || y > 250 = Nothing --delete an asteroid from the list when it is outside of the screen
            | any (($ 10) . (hit (Location x y) radius . getBulletLocation)) bs  = if radius == 10 then [] -- delete an asteroid from the list when it is hit by any bullet
                                                                                    else splitAsteroid w a
            | otherwise                                                         = [Asteroid (Middle newX newY) radius v direction]
                where
                    newX    | x > 510 = -500 
                            | x < -510 = 510
                            | otherwise = x + vx * (mag/10)
                    newY    | y > 255 = -250
                            | y < -255 = 255
                            | otherwise = y + vy * (mag/10)
                    mag = sqrt (vx * vx + vy * vy)

                    -- als de asteroid split
                   



-- get the location of a bullet
getBulletLocation :: Bullet -> Location
getBulletLocation (Bullet (Location x y) _ _) = Location x y

{-
splitAsteroid :: StdGen -> Asteroid -> [Asteroid]
splitAsteroid s a@(Asteroid m radius (Vector2d vx vy) direction) = [asteroid1, asteroid2]
                                                        where
                                                            asteroid1 = Asteroid m (radius/2) (Vector2d newVX1 newVY1) direction
                                                            asteroid2 = Asteroid m (radius/2) (Vector2d newVX2 newVY2) direction

                                                            newVX1 = vx * x_Dir1
                                                            newVY1 = yx * y_Dir1

                                                            newVX2 = vx * x_Dir2
                                                            newVY2 = yx * y_Dir2

                                                            (x_Dir2, y_Dir2)    = degreeToVector degree2
                                                            (degree2, nextG2)   = randomR(0,360) nextG2
                                                            (x_Dir1, y_Dir1)    = degreeToVector degree1
                                                            (degree1, nextG1)   = randomR (0, 360) s -}

splitAsteroid :: World -> Asteroid -> [Asteroid]
splitAsteroid w@World{asteroids = as, seed = s} a@(Asteroid m radius (Vector2d vx vy) direction)
                                        = [asteroid1, asteroid2] 
                                            where
                                                asteroid1 = Asteroid m (radius/2) (Vector2d newVX1 newVY1) direction
                                                asteroid2 = Asteroid m (radius/2) (Vector2d newVX2 newVY2) direction

                                                newVX1 = vx * x_Dir1
                                                newVY1 = vy * y_Dir1

                                                newVX2 = vx * x_Dir2
                                                newVY2 = vy * y_Dir2

                                                (x_Dir2, y_Dir2)    = degreeToVector degree2
                                                (degree2, nextG2)   = randomR(0,360) nextG1
                                                (x_Dir1, y_Dir1)    = degreeToVector degree1
                                                (degree1, nextG1)   = randomR (0, 360) s



-- | new asteroids spawnen
-- Spawns a new asteroid every 3 seconds
-- we kunnen evt de check (time - lastAs) > 3 verplaatsen naar een andere plek en deze functie áltijd gebruiken 
-- voor het spawnen van een nieuwe asteroid
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
                                               

