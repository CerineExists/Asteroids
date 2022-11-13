

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
updateAsteroid a@(Asteroid (Location x y) radius v@(Vector2d vx vy) speed)=
      Asteroid (Location newX newY) radius v speed
      where
                    newX    | x > 510 = -500 
                            | x < -510 = 510
                            | otherwise = x + vx * (mag/10)
                    newY    | y > 255 = -250
                            | y < -255 = 255
                            | otherwise = y + vy * (mag/10)
                    mag = sqrt (vx * vx + vy * vy)


-- __________________________________________________________________________________________________________________



-- | Functions below are for asteroid that were hit

splitAsteroids :: StdGen -> [Asteroid] -> (StdGen, [Asteroid])
splitAsteroids seed = splitAsteroids' seed []
  where
    splitAsteroids' seed acc [] = (seed, acc)
    splitAsteroids' seed acc (x:xs) = let
      (newSeed, asteroids) = splitAsteroid seed x
      in splitAsteroids' newSeed (asteroids++acc) xs


splitAsteroid :: StdGen -> Asteroid -> (StdGen, [Asteroid])
splitAsteroid seed a@(Asteroid m radius (Vector2d vx vy) speed)
                                  | radius <= 10  = (seed, [])
                                  | otherwise     = (nextG4,[asteroid1, asteroid2])
                                            where
                                                asteroid1 = Asteroid m (radius / 2) (Vector2d newVX1 newVY1) newSpeed1
                                                asteroid2 = Asteroid m (radius / 2) (Vector2d newVX2 newVY2) newSpeed2

                                                -- Find new directions for the asteroids
                                                newVX1 = newSpeed1 * x_Dir1
                                                newVY1 = newSpeed1 * y_Dir1

                                                newVX2 = newSpeed2 * x_Dir2
                                                newVY2 = newSpeed2 * y_Dir2

                                                -- increasing the speed of the asteroids
                                                newSpeed1 = speed * factor1
                                                newSpeed2 = speed * factor2
                                                (factor1, nextG4) = randomR(1.1, 1.5) nextG3
                                                (factor2, nextG3) = randomR(1.1, 1.5) nextG2


                                                Vector2d x_Dir2 y_Dir2 = degreeToVector degree2
                                                (degree2, nextG2)   = randomR(0,360) nextG1
                                                Vector2d x_Dir1 y_Dir1  = degreeToVector degree1
                                                (degree1, nextG1)   = randomR (0, 360) seed
                                              
