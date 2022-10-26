-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List (findIndex, elemIndex)
import Debug.Trace (trace)
import Data.Foldable
import GHC.IO.Device (IODeviceType(Directory))




-- | Handle user input
input :: Event -> World -> IO World
input e wrld = return (inputKey e w)
    where w = {-trace (show wrld)-} wrld

inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@(World (Player l d v) keys as)  = undefined --todo shoot\
inputKey (EventKey (Char c) Down _ _) w@(World (Player l d v ) keys as)  = w {keys = c : keys}
inputKey (EventKey (Char c) Up _ _)   w@(World (Player l d v ) keys as)  = w {keys = pop c keys}
inputKey _ w = w


-- | Calculate the new direction and location of the player
pop ::  Eq a =>  a -> [a] -> [a]
pop e xs = case elemIndex e xs of
  Nothing -> undefined --never happens
  Just n -> take n xs ++ drop (n+1) xs

findNewLocation :: Location -> Direction -> Velocity -> Location -- location direction velocity
findNewLocation (Location x y) (Vector2d vx vy) (Vector2d mx my) = Location newX newY
            where
                newX | x < -50 = 50 + mx 
                     | x > 50 = -50 + mx
                     | otherwise = x + mx
                newY |  y < -25 = 25 + my
                     | y > 25 = -25 + my
                     | otherwise = y + my





-- | Calculate the movements of the asteroids
adjustAsteroidList :: World -> [Asteroid] -> [Asteroid] -- return the new list of asteroids
adjustAsteroidList _ [] = []
adjustAsteroidList world (x:xs)     | isItNothing (flyingAsteroids world x) = adjustAsteroidList world xs -- delete an asteroid from the list when it is outside of the screen
                                    | otherwise = noJust (flyingAsteroids world x) : adjustAsteroidList world xs

noJust :: Maybe Asteroid -> Asteroid
noJust (Just a) = a
noJust Nothing = undefined --never happens

isItNothing :: Maybe Asteroid -> Bool
isItNothing Nothing = True
isItNothing _ = False

flyingAsteroids :: World -> Asteroid -> Maybe Asteroid
flyingAsteroids (World (Player location (Vector2d vx vy) _) _ _) a@(Asteroid (Middle x y) radius velocity direction)   
                | x < -50 = Nothing  
                | x > 50 = Nothing
                | y < -25 = Nothing
                | y > 25 = Nothing
                | isItNothing (collision a location) = Nothing -- NEE JE MOET DOOD GAAN -> NOG IMPLEMENTEREN
                | otherwise = Just (Asteroid (Middle newX newY) radius velocity direction)
                    where
                        newX = x + vx * (mag/10)
                        newY = y + vy * (mag/10)
                        mag = sqrt (vx * vx + vy * vy)


collision :: Asteroid -> Location ->  Maybe Asteroid
collision a@(Asteroid m@(Middle x1 y1) radius _ _) (Location x2 y2)     | outsideSquare m radius x2 y2 = Just a
                                                                        | otherwise = Nothing


outsideSquare :: Middle -> Float -> Float -> Float -> Bool
outsideSquare (Middle x1 y1) r x2 y2    | x2 < (x1 + r) && x2 > (x1 - r) &&  y2 < (y1 + r) && y2 > (y1 - r) = False -- de case
                                        | otherwise = True

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


stepForward :: World -> World
stepForward w@(World (Player l d v) k a) = w {player = Player (findNewLocation l d v) d v}

stepLeft :: World -> World
stepLeft w@(World (Player l d v) k a) = w {player = Player l  (d `turn` 1) v}

stepRight:: World -> World
stepRight w@(World (Player l d v) k a) = w {player = Player l (d `turn` (-1)) v}


step :: Float -> World -> IO World
step _ w@(World (Player (Location x y) (Vector2d vx vy) (Vector2d mx my)) keys as) = do -- todo change momentum
    return $ g $ foldr f w keys
    where
        f ::  Char -> World -> World
        f 'w' = stepForward
        f 'a' = stepLeft
        f 'd' = stepRight
        f  _  = id

        g :: World -> World
        g w@(World (Player (Location x y) (Vector2d vx vy) (Vector2d mx my)) k as) = 
            w {player = Player (Location (x+mx) (y+my)) (Vector2d vx vy) (Vector2d (clamp (-0.1) 0.1 (mx+vx)) (clamp (-0.1) 0.1 (my+vy)))}

-- this func makes sure a value is between a min and max value
clamp :: Float -> Float -> Float -> Float
clamp min' max' val = max min' (min max' val) 

