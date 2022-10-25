-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List (findIndex, elemIndex)
import Debug.Trace (trace)
import Data.Foldable



-- | Handle user input
input :: Event -> World -> IO World
input e wrld = return (inputKey e w)
    where w = {-trace (show wrld)-} wrld

inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@(World (Player l d v) keys)  = undefined --todo shoot
inputKey (EventKey (Char c) Down _ _) w@(World (Player l d v ) keys)  = w {keys = c : keys}
inputKey (EventKey (Char c) Up _ _)   w@(World (Player l d v ) keys)  = w {keys = pop c keys}
inputKey _ w = w

pop ::  Eq a =>  a -> [a] -> [a]
pop e xs = case elemIndex e xs of
  Nothing -> undefined --never happens
  Just n -> take n xs ++ drop (n+1) xs

findNewLocation :: Location -> Vector2d -> Vector2d -> Location
findNewLocation (Location x y) (Vector2d vx vy) (Vector2d mx my) = Location newX newY
            where
                newX = x + vx
                newY = y + vy

-- degreeToVector :: Float -> (Float, Float)
-- degreeToVector degree = normalize (x, y)
--                             where
--                                 x = sin radians
--                                 y = cos radians
--                                 radians = degree * (pi / 180)


normalize :: Vector2d -> Vector2d
normalize (Vector2d x y) = Vector2d newX newY
                        where
                            newX = x * multiplicationFactor
                            newY = y * multiplicationFactor
                            lengthVector = sqrt $ x * x + y * y 
                            multiplicationFactor = 1 / lengthVector
v1 = Vector2d 1 1
v2 = Vector2d 1 (-1)

turn :: Vector2d -> Float -> Vector2d
v@(Vector2d x y) `turn` f = Vector2d newX newY where
    mag = sqrt(x*x + y*y)
    ang = angle v
    newX = mag * cos(pi/180 * (ang + f))
    newY = mag * sin(pi/180 * (ang + f))



-- float degree = 70.0f;
-- float radians = degree * (Mathf.PI / 180);
-- Vector3 degreeVector = new Vector3(Mathf.Cos(radians), Mathf.Sin(radians), 0);

stepForward :: World -> World
stepForward w@(World (Player l d v) k) = w {player = Player (findNewLocation l d v) d v}

stepLeft :: World -> World
stepLeft w@(World (Player l d v) k) = w {player = Player l  (d `turn` 10) v}

stepRight:: World -> World
stepRight w@(World (Player l d v) k) = w {player = Player l (d `turn` (-10))    v}

step :: Float -> World -> IO World
step _ w@(World (Player l d v) keys) = 
    return $ foldr f w keys 
    where 
        f ::  Char -> World -> World
        f 'w' = stepForward 
        f 'a' = stepLeft     
        f 'd' = stepRight    
        f  _  = id 


