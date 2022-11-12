-- | Enemy

module Enemy where

import Model
import HelpFunctions
import Bullet

import Data.Foldable (Foldable(foldr'))
import System.Random
import Data.Maybe
import Data.List


bulletsOf :: UFO -> [Bullet]
bulletsOf u@UFO {bulletsUFO = bs} = bs

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



-- | Movements of the UFO
moveUFO :: UFO -> Player -> UFO
moveUFO u@UFO {locationUFO = locU, velocityUFO = velU} p@Player {location = locP} =  u {locationUFO = findNewLocation locU velU}


newVelocity :: UFO -> Location -> UFO
newVelocity u@UFO {locationUFO = locU, velocityUFO = velU, speedUFO = speed} locP@(Location lx ly) = u {velocityUFO = newVelocity}
                                                                                where
                                                                                  newDirection = mkVector locU locP
                                                                                  newVelocity = multiplyVec speed newDirection


standStill :: UFO -> UFO
standStill u = u{velocityUFO = Vector2d 0 0}


multiplyVec :: Float -> Vector2d -> Vector2d
multiplyVec speed (Vector2d x y) = Vector2d (speed*x) (speed*y)











-- | Functions below are for killing the UFO's
didABulletHitUFO :: [Bullet] -> UFO -> ([Bullet], UFO)
didABulletHitUFO bs ufo  = (newBullets, newUFO) 
                         where
                              (newBullets, hitUFO) = didBulletHitMe bs ufo  -- check if a bullet hit an UFO 
                              newUFO  | hitUFO = ufoIsKilled ufo           -- change the state of the hit UFO to 'Killed'
                                      | otherwise = ufo
                              

ufoIsKilled :: UFO -> UFO
ufoIsKilled ufo@UFO {stateUFO = s} = ufo {stateUFO = Killed}




didBulletHitMe :: [Bullet] -> UFO -> ([Bullet], Bool)
didBulletHitMe [] _ = ([], False)
didBulletHitMe (b@Bullet{locationB = locB}:bs) ufo@UFO{locationUFO = locUFO, size = rUFO}  
                                              | hit locB 5 (locUFO, rUFO*8)   = (bs, True)            -- if bullet hit the UFO
                                              | otherwise                   = (b : bullets, bools)  -- b did not hit the UFO
                                                where
                                                  (bullets, bools) = didBulletHitMe bs ufo



