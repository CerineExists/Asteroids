

module Asteroid where

import Model
import HelpFunctions




-- | Adjust every asteroid in the list
adjustAsteroidList :: World -> [Asteroid] -> [Asteroid] -- return the new list of asteroids
adjustAsteroidList _ [] = []
adjustAsteroidList world (x:xs)     | isItNothing (flyingAsteroids world x) = adjustAsteroidList world xs -- delete an asteroid from the list when it is outside of the screen
                                    | otherwise = noJust (flyingAsteroids world x) : adjustAsteroidList world xs

-- | Check per asteroid what it's new location is
flyingAsteroids :: World -> Asteroid -> Maybe Asteroid
flyingAsteroids (World (Player location _ _) _ _ bs) a@(Asteroid (Middle x y) radius v@(Vector2d vx vy) direction)   
                | x < -50 = Nothing  
                | x > 50 = Nothing
                | y < -25 = Nothing
                | y > 25 = Nothing
                | a `isHit` bs = Nothing
                | otherwise = Just (Asteroid (Middle newX newY) radius v direction)
                    where
                        newX = x + vx * (mag/10)
                        newY = y + vy * (mag/10)
                        mag = sqrt (vx * vx + vy * vy)



-- | Check if the asteroid is hit by one of the many bullets
isHit :: Asteroid -> [Bullet] -> Bool
isHit _ [] = False
isHit a (Bullet loc _ _: bs)    | collision a loc = True
                                | otherwise = isHit a bs

-- | Check if there is a collision between one bullet and one asteroid
collision :: Asteroid -> Location ->  Bool
collision a@(Asteroid m@(Middle x1 y1) radius _ _) (Location x2 y2)     | outsideSquare m radius x2 y2 = False
                                                                        | otherwise = True


outsideSquare :: Middle -> Float -> Float -> Float -> Bool
outsideSquare (Middle x1 y1) r x2 y2    | x2 < (x1 + r) && x2 > (x1 - r) &&  y2 < (y1 + r) && y2 > (y1 - r) = False -- de case
                                        | otherwise = True

