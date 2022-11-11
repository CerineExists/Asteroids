-- | Enemy

module Enemy where

import Model
import HelpFunctions

import Data.Foldable (Foldable(foldr'))
import System.Random
import Data.Maybe
import Data.List

isThereAnActiveUFO :: [UFO] -> Maybe UFO
isThereAnActiveUFO [] = Nothing
isThereAnActiveUFO (x@UFO{stateUFO = s}:xs)   | s == Attacking = Just x
                                              | otherwise = isThereAnActiveUFO xs


-- | Shooting of the UFO
shootingUFO :: World -> UFO -> World
shootingUFO  w@World{player = p@Player {location = loc}, bullets = bs, enemies = ufos, elapsedTime = time}  = undefined







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
                                              | hit locB 5 (locUFO, rUFO)   = (bs, True)            -- if bullet hit the UFO
                                              | otherwise                   = (b : bullets, bools)  -- b did not hit the UFO
                                                where
                                                  (bullets, bools) = didBulletHitMe bs ufo



