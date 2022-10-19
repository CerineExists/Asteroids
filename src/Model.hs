-- | This module contains the data types
--   which represent the state of the game
module Model where 

data World = World Player [Asteroid]       --heb er newtype van gemaakt, want dat zei hLint

data Asteroid = Asteroid Middle Radius
data Middle = Middle Float Float --x y coordinates of the middle point
type Radius = Float

data Player = Player { location :: Location, direction :: Direction }
data Location = Location Float Float 
type Direction = Float  --Direction in degrees


