-- | This module defines player movement

module Player where
import Model
import HelpFunctions
import Data.List (elemIndex)


-- | Find the new location of the player based on it's current location, direction and velocity
findNewLocation :: Location -> Direction -> Velocity -> Location 
findNewLocation (Location x y) _ (Vector2d mx my) = Location newX newY
            where
                newX | x < -500 = 500 + mx 
                     | x > 500 = -500 + mx
                     | otherwise = x + mx
                newY |  y < -250 = 250 + my
                     | y > 250 = -250 + my
                     | otherwise = y + my

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
     w {player = Player (findNewLocation l d v) (d `turn` (if 'a' `elem` k then 10 else if 'd' `elem` k then -10 else 0)) v}

