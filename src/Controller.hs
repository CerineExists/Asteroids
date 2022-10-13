-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- ONZE CODE 

-- CONTROLLER

update                    :: GameState -> IO GameState
update g@(GameState (Player (Location x y) (Direction dx dy)))  = do c <- getChar
                                                                     case c of 
                                                                      'w'       -> return (GameState (Player (Location x (y+1)) (Direction dx dy)))
                                                                      'a'       -> return (GameState (Player (Location (x-1) y) (Direction dx dy)))
                                                                      's'       -> return (GameState (Player (Location x (y-1)) (Direction dx dy)))
                                                                      'd'       -> return (GameState (Player (Location (x+1) y) (Direction dx dy)))
                                                                      _         -> return g  
                                 