-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random



-- | Handle user input
input :: Event -> World -> IO World
input e wrld = return (inputKey e wrld)

inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeyUp) Down _ _)    (World (Player location direction ))  = World (Player (findNewLocation location direction) direction)
--inputKey (EventKey (SpecialKey KeyDown) Down _ _)  (World (Player location direction ))  = World (Player (Location locX (locY - 1)) direction)
inputKey (EventKey (SpecialKey KeyRight) Down _ _) (World (Player location direction ))  = World (Player location (direction+1))
inputKey (EventKey (SpecialKey KeyLeft) Down _ _)  (World (Player location direction ))  = World (Player location (direction-1))
inputKey _ w = w

findNewLocation :: Location -> Direction -> Location
findNewLocation (Location x y) dir = Location newX newY
                                        where
                                            newX = x + fst vector
                                            newY = y + snd vector
                                            vector = degreeToVector dir

degreeToVector :: Float -> (Float, Float)
degreeToVector degree = normalize (x, y)
                            where
                                x = cos radians
                                y = sin radians
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
step _ = return 

