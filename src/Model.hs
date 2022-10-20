-- | This module contains the data types
--   which represent the state of the game
module Model where 
import Graphics.Gloss.Interface.IO.Game (Vector)

data World = World {player::Player, keys :: [Char]} 
data Player = Player { location :: Location, direction :: Vector2d, velocity :: Vector2d}
data Location = Location {x::Float, y::Float} 
type Direction = Vector2d  
data Vector2d = Vector2d {xDir :: Float, yDir:: Float}

angle :: Vector2d -> Float
angle v@(Vector2d x y) = if x == 0 then 90 else 180/pi * tanh(y/x)

instance Show World where
    show(World player keys) = show keys