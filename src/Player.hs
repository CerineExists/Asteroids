-- | This module defines player movement

module Player where
import Model
import HelpFunctions
import Data.List (elemIndex)



-- | Find a new direction
turn :: Vector2d -> Float -> Vector2d
v@(Vector2d x y) `turn` f = Vector2d newX newY where
    mag = sqrt(x*x + y*y)
    ang = angle v
    newX = mag * cos(pi/180 * (ang + f))
    newY = mag * sin(pi/180 * (ang + f))


-- | Make a new world based on the current player, world and the keys pressed 
stepa :: World -> World
stepa w@World {player = Player l d v, keys = k } =
     w {player = Player (findNewLocation l v) (d `turn` (if 'a' `elem` k then 5 else if 'd' `elem` k then -5 else 0)) v}

