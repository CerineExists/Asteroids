-- | This module defines player movement

module Player where
import Model
import HelpFunctions
import Data.List (elemIndex)


-- | Find the new location of the player based on it's current location, direction and velocity
findNewLocation :: Location -> Direction -> Velocity -> Location 
findNewLocation (Location x y) _ (Vector2d mx my) = Location newX newY
            where
                newX | x < -50 = 50 + mx 
                     | x > 50 = -50 + mx
                     | otherwise = x + mx
                newY |  y < -25 = 25 + my
                     | y > 25 = -25 + my
                     | otherwise = y + my

-- | Find a new direction
turn :: Vector2d -> Float -> Vector2d
v@(Vector2d x y) `turn` f = Vector2d newX newY where
    mag = sqrt(x*x + y*y)
    ang = angle v
    newX = mag * cos(pi/180 * (ang + f))
    newY = mag * sin(pi/180 * (ang + f))


-- | Fix the movements of the player
stepForward :: World -> World
stepForward w@(World (Player l d v) k a _) = w {player = Player (findNewLocation l d v) d v }

stepLeft :: World -> World
stepLeft w@(World (Player l d v) k a _) = w {player = Player l  (d `turn` 10) v}

stepRight:: World -> World
stepRight w@(World (Player l d v) k a _) = w {player = Player l (d `turn` (-10)) v}