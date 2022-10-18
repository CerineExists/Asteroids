-- | This module contains the data types
--   which represent the state of the game
module Model where 
import Graphics.Gloss.Interface.IO.Game (Key)

data World = World{player::Player, keys :: [Char]} 
data Player = Player { location :: Location, direction :: Direction }
data Location = Location {x::Float, y::Float} 
type Direction = Float  --Direction in degrees



instance Show World where
    show(World player keys) = show keys