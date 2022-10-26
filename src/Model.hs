
-- | This module contains the data types
--   which represent the state of the game
module Model where 
import Debug.Trace

--data World = World Player [Asteroid]       --heb er newtype van gemaakt, want dat zei hLint
data World = World{player :: Player, keys :: [Char], asteroids :: [Asteroid], bullets :: [Bullet]} 

data Bullet = Bullet {locationB :: Location, velocityB :: Velocity, travalledDistance :: Float} -- Bullet kan maximaal 50f afleggen

data Asteroid = Asteroid {middle:: Middle, radius :: Radius, velocityA :: Velocity, directionA :: Direction}
data Middle = Middle Float Float --x y coordinates of the middle point
type Radius = Float
data Player = Player { location :: Location, direction :: Direction, velocity :: Velocity} deriving Show
data Location = Location {x::Float, y::Float} deriving Show 
type Direction = Vector2d  
type Velocity = Vector2d
data Vector2d = Vector2d {xDir :: Float, yDir:: Float} deriving (Show, Eq)

angle :: Vector2d -> Float
angle v@(Vector2d x y) |x == 0    = 90 
                       |x < 0     = 180 + a
                       |y < 0     = 360 + a
                       |otherwise =  a
                       where a = 180/pi * atan(y/x)

instance Show World where
    show (World (Player (Location x y) direction v) keys asteroids _) = "Position Player: " ++ show x ++ "  " ++ show y ++ show (asteroidszien asteroids)

asteroidszien :: [Asteroid] -> String
asteroidszien = concatMap oneAsteroid

oneAsteroid :: Asteroid -> String
oneAsteroid (Asteroid (Middle x y) r v dir) = "       -- | midden: " ++ show x ++ ", " ++ show y ++ "    radius: " ++ show r
