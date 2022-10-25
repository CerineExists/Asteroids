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



-- | Handle user input
input :: Event -> World -> IO World
input e wrld = return (inputKey e w)
    where w = trace (show wrld) wrld

inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@(World (Player location direction) keys _)  = w {player = Player (findNewLocation location direction) direction}
inputKey (EventKey (Char c) Down _ _) w@(World (Player location direction ) keys _)  = w {keys = c : keys}
inputKey (EventKey (Char c) Up _ _)   w@(World (Player location direction ) keys _)  = w {keys = pop c keys}
inputKey _ w = w


-- | Calculate the new direction and location of the player
pop ::  Eq a =>  a -> [a] -> [a]
pop e xs = case elemIndex e xs of
  Nothing -> undefined --never happens
  Just n -> take n xs ++ drop (n+1) xs

findNewLocation :: Location -> Direction -> Location
findNewLocation (Location x y) dir = Location newX newY
                                        where -- check for x and y if they are outside of the screen. Then the player has to come back at the other side of the screen
                                            newX    | x < -50 = 50 + fst vector  
                                                    | x > 50 = -50 + fst vector
                                                    | otherwise = x + fst vector
                                            newY    | y < -25 = 25 + fst vector
                                                    | y > 25 = -25 + fst vector
                                                    | otherwise = y + snd vector
                                            vector = degreeToVector dir


-- | Calculate the movements of the asteroids
adjustAsteroidList :: World -> [Asteroid] -> [Asteroid] -- return the new list of asteroids
adjustAsteroidList _ [] = []
adjustAsteroidList world (x:xs)     | isItNothing (flyingAsteroids world x) = adjustAsteroidList world xs -- delete an asteroid from the list when it is outside of the screen
                                    | otherwise = noJust (flyingAsteroids world x) : adjustAsteroidList world xs

noJust :: Maybe Asteroid -> Asteroid
noJust (Just a) = a

isItNothing :: Maybe Asteroid -> Bool
isItNothing Nothing = True
isItNothing _ = False

flyingAsteroids :: World -> Asteroid -> Maybe Asteroid
flyingAsteroids (World (Player location _) _ _) a@(Asteroid (Middle x y) radius velocity direction)   | x < -50 = Nothing  
                                                                            | x > 50 = Nothing
                                                                            | y < -25 = Nothing
                                                                            | y > 25 = Nothing
                                                                            | isItNothing (collision a location) = Nothing -- NEE JE MOET DOOD GAAN -> NOG IMPLEMENTEREN
                                                                            | otherwise = Just (Asteroid (Middle newX newY) radius velocity direction)
                                                                                where
                                                                                    newX = x + fst vector * (velocity/10)
                                                                                    newY = y + snd vector * (velocity/10)
                                                                                    vector = degreeToVector direction


collision :: Asteroid -> Location ->  Maybe Asteroid
collision a@(Asteroid m@(Middle x1 y1) radius _ _) (Location x2 y2)     | outsideSquare m radius x2 y2 = Just a
                                                                        | otherwise = Nothing


outsideSquare :: Middle -> Float -> Float -> Float -> Bool
outsideSquare (Middle x1 y1) r x2 y2    | x2 < (x1 + r) && x2 > (x1 - r) &&  y2 < (y1 + r) && y2 > (y1 - r) = False -- de case
                                        | otherwise = True





-- | Help functions for calculating the new location
degreeToVector :: Float -> (Float, Float)
degreeToVector degree = normalize (x, y)
                            where
                                x = sin radians
                                y = cos radians
                                radians = degree * (pi / 180)


normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) = (newX, newY)
                        where
                            newX = x * multiplicationFactor
                            newY = y * multiplicationFactor
                            lengthVector = sqrt $ x * x + y * y 
                            multiplicationFactor = 1 / lengthVector




-- | Update function
step :: Float -> World -> IO World
step _ w@(World (Player (Location x y) direction) keys as) = return $ foldl f world2 keys
    where 
        newAsteroidList = adjustAsteroidList w as
        world2 = World (Player (Location x y) direction) keys newAsteroidList

        f :: World -> Char -> World
        f w 'w' = w {player = Player (findNewLocation (Location x y) direction) direction} 
        f w 'a' = World (Player (Location x y) (direction-10)) keys as
        f w 'd' = World (Player (Location x y) (direction+10)) keys as
        f w  _  = w 





