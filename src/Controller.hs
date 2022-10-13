-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- ONZE CODE 

-- CONTROLLER

update :: GameState -> IO GameState
update g@(GameState p@(Player (Location x y) (Direction dx dy)))  = do
    c <- getChar
    case c of 
     'w'       -> return (GameState p { location= Location  x   (y+1) })
     'a'       -> return (GameState p { location= Location (x-1) y    })
     's'       -> return (GameState p { location= Location  x   (y-1) }) 
     'd'       -> return (GameState p { location= Location (x+1) y    }) 
     _         -> return g  

getLoca :: GameState -> Location
getLoca (GameState (Player l d)) = l

