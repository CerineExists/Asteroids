-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- ONZE CODE 

-- CONTROLLER


update :: GameState -> IO a -> GameState
update GameState (Player l d) i = undefined

