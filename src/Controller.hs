-- | This module defines how the state changes in response to time and user input

module Controller where
import Model
import Player
import Asteroid
import Bullet
import HelpFunctions
import Enemy

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.List
import Debug.Trace (trace)
import Data.Foldable
import System.Random
import Data.Maybe
import System.Directory
import System.FilePath.Posix (takeDirectory)



-- | Handle user input
input :: Event -> World -> IO World
input e w = return (inputKey e w)
-- | Adds (w | a | s | d) to keys on keydown, removes them on keyup 
-- | Adds bullets to a list of bullets on spacedown 
inputKey :: Event -> World -> World                                           
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@World {player = p@(Player l d _), asteroids = as, bullets = bs} = w {bullets = Bullet l (bulletVelocity (Vector2d 1 1)*d) 5 : bs} -- SHOOT (klopt het?)
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
step time  w@(World (Player (Location x y) (Vector2d dx dy) (Vector2d vx vy)) keys as bullets state score pics seed enemies activeUFO _) = do -- todo change momentum
     case state of 
      Playing -> return $ (adjustTime . adjustScore . adjustEnemies . bulletsAndAsteroids . momentum . foldr move w) keys
      Dead    -> gameover w
      _       -> return w
    where
        -- Moves the player in accordance with the characters in the keys list
        move ::  Char -> World -> World
        move c = stepa
        -- Changes the momentum with respect to velocity
        momentum :: World -> World
        momentum w@World{player = Player (Location x y          ) (Vector2d dx dy) (Vector2d vx                      vy)} =
                 w      {player = Player (Location (x+vx) (y+vy)) (Vector2d dx dy) (Vector2d (clamp 30 ((vx+dx)/2)) (clamp 30 ((dy+vy)/2)))}

        adjustTime :: World -> World
        adjustTime w@World {elapsedTime = t} = w {elapsedTime = t + time}

       
-- | It adjusts the enemies (UFO's) and their corresponding ufoBullets
adjustEnemies :: World -> World
adjustEnemies w@World{player = p@Player {location = loc}, bullets = bs, enemies = ufos, elapsedTime = time} 
                                              | isNothing maybeUFO = w -- if there is no active UFO, return immediately
                                              | otherwise = w {bullets = newBullets, enemies = newUFOS}
                                                      where 
                                                        -- did a bullet hit the active UFO?
                                                        (newBullets, newUFO1) = didABulletHitUFO bs (fromJust maybeUFO) 
                                                        -- Calculate new location for the UFO:
                                                        newUFO2 = moveUFO newUFO1 p 
                                                        -- Adjust the velocity based on where the player is:
                                                        newUFO3 | not $ minimumDistance newUFO2 loc = standStill newUFO2
                                                                | otherwise                       = newVelocity newUFO2 loc                                                         
                                                        newUFO4 = shootingUFO w newUFO3 -- nieuwe kogels van de ufo
                                                        newUFOS = [newUFO4]                                     
                                                        
                                                        maybeUFO = isThereAnActiveUFO ufos -- check if there is an active ufo

                                                        -- nog spawnen van ufo's implementeren
                                                        -- Eerste UFO pas laten komen als er minimaal ?300? punten zijn gehaald. 
                                                        -- Tweede UFO bij een minimum van ?600? punten EN DE VORIGE UFO MOET DOOD ZIJN
                                                        -- (DAT IS HEEL BELANGRIJK, WANT ALLES IS EROP GEBOUWD DAT ER MAAR 1 UFO TEGELIJK ACTIEF KAN ZIJN)
                                                        -- Derde UFO ook pas laten spawnen als vorige UFO dood is
                                           
                                                       
                                                        

minimumDistance :: UFO -> Location -> Bool
minimumDistance ufo@UFO{locationUFO = loc@(Location x1 y1)} (Location x2 y2)  | sqrt (dXSquare + dYSquare) >= 200 = True
                                                                              | otherwise = False
                                                              where
                                                                dXSquare = deltaX * deltaX
                                                                dYSquare = deltaY * deltaY
                                                                deltaX = x2 - x1
                                                                deltaY = y2 - y1

                                      



-- Adjusts the list of asteroids and bullets in World
bulletsAndAsteroids :: World -> World
bulletsAndAsteroids w@World{seed = seed, bullets = bs, asteroids = as, score = score, state = state, enemies = ufos} = 
                         w {seed = newSeed, bullets = newLocBullets, asteroids = newAsteroids ++ newLocAsteroids,  score = score + newScore, state = newState}
            where
              --check if the player hit an asteroid or ufos bullet
              newState = if any (playerHitAsteroid (player w)) as || any (playerHitUfoBullet (player w)) (ufos)  then Dead else state
              -- first remove the bullets that hit an asteroid and seperate the hit asteroids from the not hit asteroids
              (notHitBullets, hitAsteroids, notHitAsteroids) = didBulletHitAsteroid bs as
              -- give the bullets that did not hit anything a new location
              newLocBullets = adjustBulletLocations notHitBullets
              -- give the asteroids that were not hit a new location
              newLocAsteroids = adjustAsteroidLocations notHitAsteroids
              -- split the asteroids that were hit
              (newSeed, newAsteroids) = splitAsteroids seed hitAsteroids
              -- new score calculation
              newScore = asteroidPoints hitAsteroids

-- get 100 points for every asteroid you hit
asteroidPoints :: [Asteroid] -> Int
asteroidPoints asteroids = length asteroids * 100
   

-- | Checks if a bullet hit an asteroid and deletes that bullet
didBulletHitAsteroid :: [Bullet] -> [Asteroid] -> ([Bullet], [Asteroid], [Asteroid]) 
didBulletHitAsteroid bs as = (newBullets, hitAsteroids, notHitAsteroids)
              where
                newBullets = mapMaybe (deleteBullet as) bs
                (hitAsteroids, notHitAsteroids) = partitionAsteroids bs as
-- functie verwijderd bullets uit de list van bullets en returnt 1) asteroids die wél en 2) asteroids die niet geraakt zijn



-- Makes sure a value is between a min and max value x and -x
clamp :: Float -> Float -> Float
clamp x val = max (-x) (min x val)

-- | adjusts the score 
adjustScore :: World -> World -- todo add enemy and asteroid death events
adjustScore w@World{score = score} = w {score = score + 1}

gameover :: World -> IO World
gameover w@World{state = s} = do
  let oldPath = "scores.txt"
  lns <- readFile oldPath
  let newPath = "newscores.txt" 
  let newLns = lns ++ "Your score was: " ++ show (score w) ++ "\n"
  createDirectoryIfMissing True $ takeDirectory newPath
  writeFile newPath newLns
  renameFile newPath oldPath 
  return w{state = Done}

-- function that checks if player hit an asteroid
playerHitAsteroid :: Player -> Asteroid -> Bool
playerHitAsteroid (Player (Location x y) _ _) (Asteroid (Middle ax ay) _ _ _) = (x-ax)^2 + (y-ay)^2 < 20^2

-- function that checks if player hit an ufo bullet
playerHitUfoBullet :: Player -> UFO -> Bool
playerHitUfoBullet p@Player {location = Location x y} u@UFO {bulletsUFO = bs} = any (playerHitBullet p) bs

playerHitBullet :: Player -> Bullet -> Bool
playerHitBullet (Player (Location x y) _ _) (Bullet (Location bx by) _ _) = (x-bx)^2 + (y-by)^2 < 20^2