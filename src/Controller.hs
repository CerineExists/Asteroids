-- | This module defines how the state changes in response to time and user input

module Controller where
import Model
import Player
import Asteroid
import Bullet
import HelpFunctions

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List (findIndex, elemIndex)
import Debug.Trace (trace)
import Data.Foldable
import System.Random


-- | Handle user input
input :: Event -> World -> IO World
input e w = return (inputKey e w)
-- | Adds (w | a | s | d) to keys on keydown, removes them on keyup 
-- | Adds bullets to a list of bullets on spacedown 
inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@(World (Player l d _) _ as bs _ _ _ _ _ _)   = w {bullets = Bullet l (bulletVelocity (Vector2d 3 3)*d) 0 : bs} -- SHOOT (klopt het?)
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) w@World {state = state}  | state == Pause = w {state = Playing}
                                                                          | otherwise = w {state = Pause}
inputKey (EventKey (Char c) Down _ _) w@World { keys = keys} = w {keys = c : keys}
inputKey (EventKey (Char c) Up   _ _) w@World { keys = keys} = w {keys = pop c keys}
inputKey _ w                                                 = w

-- | removes an element from the list
pop ::  Eq a =>  a -> [a] -> [a]
pop e xs = case elemIndex e xs of
  Nothing -> error "You just tried to remove an element that isn't in the list" --never happens, just like your sex life
  Just n -> take n xs ++ drop (n+1) xs


-- | Update the state of the world
step :: Float -> World -> IO World
step time w@(World (Player (Location x y) (Vector2d dx dy) (Vector2d vx vy)) keys as bullets state score pics _ _ _) = do -- todo change momentum
     --print (x,y, "b: ", bullets)
     if state == Pause
        then return w
        else return $ (adjustTime . spawnNewAsteroid . adjustScore . bulletsAndAsteroids . momentum . foldr move w) keys
    where
        -- Moves the player in accordance with the characters in the keys list
        move ::  Char -> World -> World
        move c = stepa
        -- Changes the momentum with respect to velocity
        momentum :: World -> World
        momentum w@World{player = Player (Location x y          ) (Vector2d dx dy) (Vector2d vx                      vy)} =
                 w      {player = Player (Location (x+vx) (y+vy)) (Vector2d dx dy) (Vector2d (clamp 15 ((vx+dx)/2)) (clamp 15 ((dy+vy)/2)))}

        adjustTime :: World -> World
        adjustTime w@World {elapsedTime = t} = w {elapsedTime = t + time}

        -- Adjusts the list of asteroids
        bulletsAndAsteroids :: World -> World
        bulletsAndAsteroids w@World { asteroids = as }= w { asteroids = adjustAsteroidList w as, bullets   = adjustBulletList   w bullets}


-- Spawns a new asteroid every 3 seconds
-- we kunnen evt de check (time - lastAs) > 3 verplaatsen naar een andere plek en deze functie áltijd gebruiken 
-- voor het spawnen van een nieuwe asteroid
spawnNewAsteroid :: World -> World
spawnNewAsteroid w@World{asteroids = as, seed = s, elapsedTime = time, lastAsteroidSpawned = lastAs}  
                                                        | (time - lastAs) > 3 = w{seed = nextG3, asteroids = newAsteroid : as, lastAsteroidSpawned = time} 
                                                        | otherwise = w
                                            where
                                              newAsteroid :: Asteroid
                                              newAsteroid    = Asteroid findMiddle 2 velocity (Vector2d 0 0) -- (findVelocity locationAsteroid)

                                              findMiddle :: Middle 
                                              findMiddle    | locAsteroid == North  = Middle middleXY 25    -- spawn at top of screen
                                                            | locAsteroid == South  = Middle middleXY (-25)  -- spawn at bottom of screen
                                                            | locAsteroid == East   = Middle 50 middleXY  -- spawn at left of screen                                                          
                                                            | otherwise             = Middle (-50) middleXY   -- spawn at right of screen


                                              -- determine if the asteroid will spawn top, right, bottom or left
                                              locAsteroid :: LocationNewAsteroid
                                              locAsteroid | generator == 0 = North
                                                          | generator == 1 = East
                                                          | generator == 2 = South
                                                          | otherwise = West
                                                              where
                                                                generator = round (time*100) `mod` 4 

                                              -- find the random middlepoint
                                              (middleXY, nextG3)  | locAsteroid == North || locAsteroid == South = randomR ((-50), 50) nextG2
                                                                  | otherwise = randomR ((-25), 25) nextG2


                                              -- determine velocity
                                              velocity = Vector2d (speed * x_Dir) (speed*y_Dir)
                                              (speed, nextG2) = randomR (1,3) nextG1

                                              (x_Dir, y_Dir) = degreeToVector degree
                                              (degree, nextG1)     = randomR (0, 360) s
                                               

 
-- data LocationNewAsteroid = North | East | South | West    

-- Makes sure a value is between a min and max value x and -x
clamp :: Float -> Float -> Float
clamp x val = max (-x) (min x val)

-- | adjusts the score 
adjustScore :: World -> World -- todo add enemy and asteroid death events
adjustScore w@World{score = score} = w {score = score + 1}






-- | implement randomness



