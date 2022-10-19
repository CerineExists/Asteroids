-- | This module contains the data types
--   which represent the state of the game
module Model where 
import Graphics.Gloss.Interface.IO.Game (Key)

--data World = World Player [Asteroid]       --heb er newtype van gemaakt, want dat zei hLint

data Asteroid = Asteroid Middle Radius
data Middle = Middle Float Float --x y coordinates of the middle point
type Radius = Float

data World = World{player :: Player, keys :: [Char], asteroids :: [Asteroid]} 
data Player = Player { location :: Location, direction :: Direction }
data Location = Location {x::Float, y::Float} 
type Direction = Float  --Direction in degrees



instance Show World where
    show(World player keys _) = show keys