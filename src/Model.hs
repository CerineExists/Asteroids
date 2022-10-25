-- | This module contains the data types
--   which represent the state of the game
module Model where 
import Debug.Trace


data World = World {player::Player, keys :: [Char]} deriving Show
data Player = Player { location :: Location, direction :: Vector2d, velocity :: Vector2d} deriving Show
data Location = Location {x::Float, y::Float} deriving Show 
type Direction = Vector2d  
data Vector2d = Vector2d {xDir :: Float, yDir:: Float} deriving Show

angle :: Vector2d -> Float
angle v@(Vector2d x y) |x == 0    =  90 
                       |otherwise =  180/pi * atan(y/x)
