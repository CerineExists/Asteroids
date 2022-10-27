
-- | This module contains the data types which represent the state of the game
module Model where 
import Debug.Trace
import Graphics.Gloss (Picture)

-- | data type World which contains the whole gamestate
data World = World{ player :: Player,  --todo add the standard pictures to world
                    keys :: [Char], 
                    asteroids :: [Asteroid], 
                    bullets :: [Bullet],
                    state :: State,
                    score :: Int,
                    pics :: [Picture]
                    } 


data State = Playing | Pause deriving Eq

-- | data type Asteroid
data Asteroid = Asteroid {middle:: Middle, radius :: Radius, velocityA :: Velocity, directionA :: Direction}

-- | data type Player
data Player = Player { location :: Location, direction :: Direction, velocity :: Velocity} deriving Show

-- | data type Bullet
data Bullet = Bullet {locationB :: Location, velocityB :: Velocity, travalledDistance :: Float} -- Bullet kan maximaal 50f afleggen

-- | (data)types that are used in the datatypes above
data Middle = Middle Float Float -- x y coordinates of the middle point
type Radius = Float
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


-- | helper functions for the programmer to see what happens in the application
instance Show World where
    show (World (Player (Location x y) direction v) keys asteroids _ _ _ _) = "Position Player: " ++ show x ++ "  " ++ show y ++ show (asteroidszien asteroids)

asteroidszien :: [Asteroid] -> String
asteroidszien = concatMap oneAsteroid

oneAsteroid :: Asteroid -> String
oneAsteroid (Asteroid (Middle x y) r v dir) = "       -- | midden: " ++ show x ++ ", " ++ show y ++ "    radius: " ++ show r
