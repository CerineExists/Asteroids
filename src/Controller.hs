-- | This module defines how the state changes in response to time and user input
module Controller where
import Model

-- | other modules that are part of the Controller layer:
import Player
import Asteroid
import Bullet



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
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@(World (Player l d v) keys as bullets)  = w {bullets = Bullet l (bulletVelocity d) 0 : bullets} -- SHOOT (klopt het?)
inputKey (EventKey (Char c) Down _ _) w@(World (Player l d v ) keys as _)  = w {keys = c : keys}
inputKey (EventKey (Char c) Up _ _)   w@(World (Player l d v ) keys as _)  = w {keys = pop c keys}
inputKey _ w = w

-- | ??? JACK ???
pop ::  Eq a =>  a -> [a] -> [a]
pop e xs = case elemIndex e xs of
  Nothing -> undefined --never happens
  Just n -> take n xs ++ drop (n+1) xs


-- | Update the state of the world
step :: Float -> World -> IO World
step _ w@(World (Player (Location x y) (Vector2d dx dy) (Vector2d vx vy)) keys as bullets) = do -- todo change momentum
    --print (dx, dy)
    return $ b' $ a' $ g $ foldr f w keys --return $ (a' . g) $ foldr f w keys
    where
        f ::  Char -> World -> World
        f 'w' = stepForward
        f 'a' = stepLeft
        f 'd' = stepRight
        f  _  = id

        g :: World -> World
        g w@(World (Player (Location x y) (Vector2d dx dy) (Vector2d vx vy)) _ _ _) = 
            w {player = Player (Location (x+vx) (y+vy)) (Vector2d dx dy) (Vector2d (clamp 0.1 ((vx+dx)/2)) (clamp 0.1 ((dy+vy)/2)))}

        a' :: World -> World
        a' w@(World _ _ as _) = 
            w {asteroids = adjustAsteroidList w as}
        
        b' :: World -> World
        b' w@(World _ _ _ bullets) = 
            w { bullets = adjustBulletList w bullets}

-- this func makes sure a value is between a min and max value x and -x
clamp :: Float -> Float -> Float
clamp x val = max (-x) (min x val)

