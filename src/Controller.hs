-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
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

pop ::  Eq a =>  a -> [a] -> [a]
pop e xs = case elemIndex e xs of
  Nothing -> undefined --never happens
  Just n -> take n xs ++ drop (n+1) xs

findNewLocation :: Location -> Direction -> Location
findNewLocation (Location x y) dir = Location newX newY
                                        where
                                            newX = x + fst vector
                                            newY = y + snd vector
                                            vector = degreeToVector dir

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


-- float degree = 70.0f;
-- float radians = degree * (Mathf.PI / 180);
-- Vector3 degreeVector = new Vector3(Mathf.Cos(radians), Mathf.Sin(radians), 0);

step :: Float -> World -> IO World
step _ w@(World (Player (Location x y) direction) keys as) = return $ foldl f w keys
    where 
        f:: World -> Char -> World
        f w 'w' = w {player = Player (findNewLocation (Location x y) direction) direction} 
        f w 'a' = World (Player (Location x y) (direction-10)) keys as
        f w 'd' = World (Player (Location x y) (direction+10)) keys as
        f w  _  = w 




-- | Random rotsblokken
--komtErEenRotsblok :: Maybe Asteroid
--komtErEenRotsblok = do

