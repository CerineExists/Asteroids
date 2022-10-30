

module Asteroid where

import Model
import HelpFunctions
import Bullet
import Data.Foldable (Foldable(foldr'))




-- | Adjust every asteroid in the list
adjustAsteroidList :: World -> [Asteroid] -> [Asteroid] -- return the new list of asteroids
adjustAsteroidList _ [] = []
adjustAsteroidList world (x:xs) | isItNothing (flyingAsteroids world x) = adjustAsteroidList world xs -- delete an asteroid from the list when it is outside of the screen
                                | otherwise = noJust (flyingAsteroids world x) : adjustAsteroidList world xs

-- | Check per asteroid what it's new location is
flyingAsteroids :: World -> Asteroid -> Maybe Asteroid
flyingAsteroids (World (Player location _ _) _ _ bs _ _ _ _ _ _) a@(Asteroid (Middle x y) radius v@(Vector2d vx vy) direction)
            -- | x < -500  || x > 500 || y < -250 || y > 250 = Nothing --delete an asteroid from the list when it is outside of the screen
            | any (($ 10) . (hit (Location x y) radius . getBulletLocation)) bs = Nothing -- delete an asteroid from the list when it is hit by any bullet
            | otherwise                                                        = Just (Asteroid (Middle newX newY) radius v direction)
                where
                    newX = x + vx * (mag/10)
                    newY = y + vy * (mag/10)
                    mag = sqrt (vx * vx + vy * vy)

-- get the location of a bullet
getBulletLocation :: Bullet -> Location
getBulletLocation (Bullet (Location x y) _ _) = Location x y
