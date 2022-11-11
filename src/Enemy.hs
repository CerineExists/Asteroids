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



-- gebruik intelligentie!! Ga richting P bewegen
moveUFO :: UFO -> Player -> UFO
moveUFO u@UFO {locationUFO = locU, velocityUFO = velU} p@Player {location = locP} =  u {locationUFO = findNewLocation locU velU}














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



