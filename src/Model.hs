-- | This module contains the data types
--   which represent the state of the game
module Model where 

newtype World = World Player  --heb er newtype van gemaakt, want dat zei hLint


data Player = Player { location :: Location, direction :: Direction }
data Location = Location Float Float 
type Direction = Float  --Direction in degrees


