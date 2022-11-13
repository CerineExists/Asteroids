-- | Enemy

module Enemy where

import Model
import HelpFunctions
import Bullet

import Data.Foldable (Foldable(foldr'))
import System.Random
import Data.Maybe
import Data.List


-- __________________________________________________________________________________________________________________

-- | Manage the UFOs: Is it time to activate one?                                                       
manageUFOs :: World -> World
manageUFOs w@World{enemies = ufos, elapsedTime = time, score = s} 
                        | s >= 400 && isNothing maybeUFO && stateOfUFO (head ufos)  == Waiting = w {enemies = activateUFO ufos 0} -- activeer ufo 1
                        | s >= 800 && isNothing maybeUFO && stateOfUFO (ufos !! 1)  == Waiting = w {enemies = activateUFO ufos 1} -- activeer ufo 2
                        | s >= 1200 && isNothing maybeUFO && stateOfUFO (ufos !! 2) == Waiting = w {enemies = activateUFO ufos 2} -- activeer ufo 3
                        | otherwise = w
                              where
                                maybeUFO = isThereAnActiveUFO ufos

-- | Activate UFO in a list
activateUFO :: [UFO] -> Int -> [UFO]
activateUFO ufos i = replace ufos i toBeActivated{stateUFO = Attacking}
                             where 
                                toBeActivated = ufos !! i

-- | Return state of the UFO                     
stateOfUFO :: UFO -> StateUFO  
stateOfUFO u@UFO {stateUFO = s} = s    


-- | replaces the i'th element of ufos with one ufo
replace :: [UFO] -> Int -> UFO -> [UFO]   
replace ufos i new = take i ufos ++ [new] ++ drop (i+1) ufos  


-- __________________________________________________________________________________________________________________



-- | It adjusts the UFO that is active and it's corresponding ufoBullets
adjustEnemies :: World -> World
adjustEnemies w@World{player = p@Player {location = loc}, bullets = bs, enemies = ufos, elapsedTime = time} 
                                              | isNothing maybeUFO  = w -- if there is no active UFO, return immediately
                                              | otherwise           = w {bullets = newBullets, enemies = newUFOS}
                                                      where 
                                                        -- did a bullet hit the active UFO?
                                                        -- if it did newUFO1 has the state Killed
                                                        (newBullets, newUFO1) = didABulletHitUFO bs (fromJust maybeUFO) 

                                                        -- Calculate new location for the UFO:
                                                        newUFO2 = moveUFO newUFO1 p 

                                                        -- Adjust the velocity based on where the player is:
                                                        newUFO3 | not $ minimumDistance newUFO2 loc = standStill newUFO2
                                                                | otherwise                         = newVelocity newUFO2 loc  

                                                        -- Adjust the bullets of the UFO                                                               
                                                        newUFO4 = shootingUFO w newUFO3 

                                                        -- Replace the old ufo with the new ufo (and their corresponding bullets)
                                                        newUFOS =  replace ufos number newUFO4  
                                                        number = whichNumber (fromJust maybeUFO)                                   
                                                        
                                                        -- Check if there is an active ufo
                                                        maybeUFO = isThereAnActiveUFO ufos 

minimumDistance :: UFO -> Location -> Bool
minimumDistance ufo@UFO{locationUFO = loc@(Location x1 y1)} (Location x2 y2)  | sqrt (dXSquare + dYSquare) >= 200 = True
                                                                              | otherwise = False
                                                              where
                                                                dXSquare = deltaX * deltaX
                                                                dYSquare = deltaY * deltaY
                                                                deltaX = x2 - x1
                                                                deltaY = y2 - y1

whichNumber :: UFO -> Int
whichNumber u@UFO{number = n} = n                                                                   
                                                      

-- __________________________________________________________________________________________________________________

                          

-- | Shooting of the UFO
shootingUFO :: World -> UFO -> UFO
shootingUFO w@World{player = p@Player {location = locP@(Location px py)}, elapsedTime = time} 
            u@UFO {locationUFO = locU@(Location ux uy), bulletsUFO = bs, lastShotAt = lastShot } 
                      | time - lastShot < 2 =  u {bulletsUFO = newBulletLocs} -- do NOT shot a new bullet    
                      | otherwise = u {bulletsUFO = newBullet : newBulletLocs, lastShotAt = time}
                            where
                              newBullet = Bullet locU (bulletVelocity (findBulletDirection locU locP)) 2.5
                              newBulletLocs = adjustBulletLocations bs  -- adjust the location of the bullets that are already there

findBulletDirection :: Location -> Location -> Direction
findBulletDirection (Location x1 y1) (Location x2 y2) = normalize $ Vector2d (x2 - x1) (y2 - y1)


-- __________________________________________________________________________________________________________________

-- | Movements of the UFO
moveUFO :: UFO -> Player -> UFO
moveUFO u@UFO {locationUFO = locU, velocityUFO = velU} p@Player {location = locP} =  u {locationUFO = findNewLocation locU velU}

-- | Choose a new velocity intelligently (based on the player's location)
newVelocity :: UFO -> Location -> UFO
newVelocity u@UFO {locationUFO = locU, velocityUFO = velU, speedUFO = speed} locP@(Location lx ly) = u {velocityUFO = newVelocity}
                                                                                where
                                                                                  newDirection = mkVector locU locP
                                                                                  newVelocity = multiplyVec speed newDirection


standStill :: UFO -> UFO
standStill u = u{velocityUFO = Vector2d 0 0}

-- __________________________________________________________________________________________________________________

-- | Functions below are for killing the UFO's
didABulletHitUFO :: [Bullet] -> UFO -> ([Bullet], UFO)
didABulletHitUFO bs ufo  = (newBullets, newUFO) 
                         where
                              (newBullets, hitUFO) = didBulletHitMe bs ufo    -- check if a bullet hit an UFO 
                              newUFO  | hitUFO    = ufoIsKilled ufo            -- change the state of the hit UFO to 'Killed'
                                      | otherwise = ufo
                              

ufoIsKilled :: UFO -> UFO
ufoIsKilled ufo@UFO {stateUFO = s} = ufo {stateUFO = Killed}



didBulletHitMe :: [Bullet] -> UFO -> ([Bullet], Bool)
didBulletHitMe [] _ = ([], False)
didBulletHitMe (b@Bullet{locationB = locB}:bs) ufo@UFO{locationUFO = locUFO, size = rUFO}  
                                              | hit locB 5 (locUFO, rUFO*8)   = (bs, True)              -- if bullet hit the UFO
                                              | otherwise                     = (b : bullets, bools)    -- b did not hit the UFO
                                                  where
                                                    (bullets, bools) = didBulletHitMe bs ufo



