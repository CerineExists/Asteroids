-- | This module defines how the state changes in response to time and user input

module Controller where
import Model
import Player
import Asteroid
import Bullet

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List (findIndex, elemIndex)
import Debug.Trace (trace)
import Data.Foldable


-- | Handle user input
input :: Event -> World -> IO World
input e w = return (inputKey e w)
-- | Adds (w | a | s | d) to keys on keydown, removes them on keyup 
-- | Adds bullets to a list of bullets on spacedown 
inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@(World (Player l d _) _ as bs _ _ _)   = w {bullets = Bullet l (bulletVelocity (Vector2d 3 3)*d) 0 : bs} -- SHOOT (klopt het?)
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) w@(World _ _ _ _ state _ _)                =   if state == Pause
                                                                                                then w {state = Playing}
                                                                                            else w {state = Pause}
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
step _ w@(World (Player (Location x y) (Vector2d dx dy) (Vector2d vx vy)) keys as bullets state score pics) = do -- todo change momentum
     --print (x,y, "b: ", bullets)
     if state == Pause
        then return w
        else return $ (adjustScore . bulletsAndAsteroids . momentum . foldr move w) keys
    where
        -- Moves the player in accordance with the characters in the keys list
        move ::  Char -> World -> World
        move c = stepa
        -- Changes the momentum with respect to velocity
        momentum :: World -> World
        momentum w@World{player = Player (Location x y          ) (Vector2d dx dy) (Vector2d vx                      vy)} =
                 w      {player = Player (Location (x+vx) (y+vy)) (Vector2d dx dy) (Vector2d (clamp 15 ((vx+dx)/2)) (clamp 15 ((dy+vy)/2)))}

        -- Adjusts the list of asteroids
        bulletsAndAsteroids :: World -> World
        bulletsAndAsteroids w@World { asteroids = as }= w { asteroids = adjustAsteroidList w as, bullets   = adjustBulletList   w bullets}


-- Makes sure a value is between a min and max value x and -x
clamp :: Float -> Float -> Float
clamp x val = max (-x) (min x val)

-- | adjusts the score 
adjustScore :: World -> World -- todo add enemy and asteroid death events
adjustScore w@World{score = score} = w {score = score + 1}


