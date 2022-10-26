module Bullet where 

import Model
import HelpFunctions


-- | BULLETS

-- | Adjusting the speed of a bullet 
bulletVelocity :: Direction -> Velocity
bulletVelocity (Vector2d dx dy) = Vector2d (dx*3) (dy*3)

-- | Adjust every bullet in the list
adjustBulletList ::  World -> [Bullet] -> [Bullet]
adjustBulletList _ [] = []
adjustBulletList world (x:xs)   | isItNothing (flyingBullet world x) = adjustBulletList world xs -- delete an asteroid from the list when it is outside of the screen
                                | otherwise = noJust (flyingBullet world x) : adjustBulletList world xs

-- Check per bullet what it's new location is and delete if hit something 
flyingBullet :: World -> Bullet -> Maybe Bullet
flyingBullet (World (Player location _ _) _ as _) b@(Bullet loc@(Location lx ly) velocity@(Vector2d vx vy) travalledDistance) 
                -- first check if the bullet is outside of the screen
                | lx < -60 = Nothing  
                | lx > 60 = Nothing
                | ly < -35 = Nothing
                | ly > 35 = Nothing

                -- check if the bullet has reached it maximum travel distance
                | travalledDistance >= 50 = Nothing

                -- check if the bullet hit an enemy -- bullet wordt verwijderd. EERDER IN ASTEROIDS DAN OOK DE ASTEROID SPLITTEN/VERWIJDEREN
                -- isItNothing (collision a location) = Nothing -- NEE JE MOET DOOD GAAN -> NOG IMPLEMENTEREN
                | loc `hit` as  = Nothing

                -- if the bullet still exists, than calculate the new position of the bullet
                | otherwise = Just (Bullet (Location newX newY) velocity travalledDistance)
                    where
                        newX = lx + vx * (mag/10)
                        newY = ly + vy * (mag/10)
                        mag = sqrt (vx * vx + vy * vy)

-- | Did the bullet hit an asteroid?
hit :: Location -> [Asteroid] ->  Bool -- Location = location of the bullet
hit _ [] = False
hit (Location x2 y2) (a@(Asteroid (Middle x1 y1) r _ _):as)     
                                                | x2 < (x1 + r) && x2 > (x1 - r) &&  y2 < (y1 + r) && y2 > (y1 - r) = True
                                                | otherwise = False
