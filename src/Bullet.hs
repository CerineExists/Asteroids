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
flyingBullet (World (Player location _ _) _ as _ _ _ _ _ _ _) b@(Bullet loc@(Location lx ly) velocity@(Vector2d vx vy) travalledDistance)
                | lx < -500 || lx > 500 || ly < -350 ||  ly > 350 || travalledDistance >= 100= Nothing  -- check if the bullet has reached it maximum travel distance or if it is outside of the screen  
                | any (($ 1) . (hit loc 5 . getAsteroidLocation)) as = Nothing -- delete a bullet from the list when it is hit by any asteroid
                -- check if the bullet hit an enemy -- bullet wordt verwijderd. EERDER IN ASTEROIDS DAN OOK DE ASTEROID SPLITTEN/VERWIJDEREN
                -- isItNothing (collision a location) = Nothing -- NEE JE MOET DOOD GAAN -> NOG IMPLEMENTEREN
                -- if the bullet still exists, than calculate the new position of the bullet
                | otherwise = Just (Bullet (Location newX newY) velocity travalledDistance)
                    where
                        newX = lx + vx * (mag/10)
                        newY = ly + vy * (mag/10)
                        mag = sqrt (vx * vx + vy * vy)


-- get the location of an asteroid
getAsteroidLocation :: Asteroid -> Location
getAsteroidLocation (Asteroid (Middle x y) _ _ _) = Location x y