-- | This module contains the data types which represent the state of the game
module Model where 
import Debug.Trace
import Graphics.Gloss (Picture)

import System.Random

-- | data type World which contains the whole gamestate
data World = World{ player :: Player,  --todo add the standard pictures to world
                    keys :: [Char], 
                    asteroids :: [Asteroid], 
                    bullets :: [Bullet],
                    state :: State,
                    score :: Int,
                    pics :: PicList,
                    seed :: StdGen,
                    elapsedTime :: Float,
                    lastAsteroidSpawned :: Float
                    } 


data State = Playing | Pause deriving Eq

-- | data type Asteroid
data Asteroid = Asteroid {middle:: Middle, radius :: Radius, velocityA :: Velocity, directionA :: Direction} deriving Show

-- | data type Player
data Player = Player { location :: Location, direction :: Direction, velocity :: Velocity} deriving Show

-- | data type Bullet
data Bullet = Bullet {locationB :: Location, velocityB :: Velocity, travalledDistance :: Float} deriving Show-- Bullet kan maximaal 50f afleggen

-- | (data)types that are used in the datatypes above
data Middle = Middle Float Float deriving Show-- x y coordinates of the middle point
type Radius = Float
data Location = Location {x::Float, y::Float} deriving Show 
type Direction = Vector2d  
type Velocity = Vector2d
data Vector2d = Vector2d {xDir :: Float, yDir:: Float} deriving (Show, Eq)
data PicList = PicList {lazyRocket :: Picture, runningRockets :: [Picture], space :: Picture, asteroid :: Picture}
angle :: Vector2d -> Float
angle v@(Vector2d x y) |x == 0    = 90 
                       |x < 0     = 180 + a
                       |y < 0     = 360 + a
                       |otherwise =  a
                       where a = 180/pi * atan(y/x)

data LocationNewAsteroid = North | East | South | West deriving Eq
                        

-- | Instance Num for vector arithmetic
instance Num Vector2d where
    (Vector2d x1 y1) * (Vector2d x2 y2) = Vector2d (x1*x2) (y1*y2)
    (Vector2d x1 y1) + (Vector2d x2 y2) = Vector2d (x1+x2) (y1+y2)
    (Vector2d x1 y1) - (Vector2d x2 y2) = Vector2d (x1-x2) (y1-y2)
    abs (Vector2d x y) = Vector2d (abs x) (abs y)
    signum (Vector2d x y) = Vector2d (signum x) (signum y)
    fromInteger i = Vector2d (fromInteger i) (fromInteger i)



-- | helper functions for the programmer to see what happens in the application
instance Show World where
    show World {player = Player (Location x y) d v, asteroids = as} = "Position Player: " ++ show x ++ "  " ++ show y ++ show (asteroidszien as)

asteroidszien :: [Asteroid] -> String
asteroidszien = concatMap oneAsteroid

oneAsteroid :: Asteroid -> String
oneAsteroid (Asteroid (Middle x y) r v dir) = "       -- | midden: " ++ show x ++ ", " ++ show y ++ "    radius: " ++ show r


-- checks if a location is within a radius of another location
hit :: Location -> Radius -> Location -> Radius -> Bool
hit (Location x1 y1) r1 (Location x2 y2) r2 = (x1-x2)^2 + (y1-y2)^2 <= (r1+r2)^2





-- | ANIMATION
{-
Een paar afbeeldingen die elkaar afwisselen

Stel animatie met ruimteschip:

Wat ik wil is dat ipv alleen 




-}