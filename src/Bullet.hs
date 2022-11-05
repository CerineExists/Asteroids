module Bullet where 
import Model
import HelpFunctions
import Data.Maybe
import Data.List


-- | BULLETS

-- | Check if asteroid was hit by a bullet
didABulletHitMe :: [Bullet] -> Asteroid -> Bool
didABulletHitMe bs a@(Asteroid (Middle x y) radius v@(Vector2d vx vy) _)
      | or boolList = True -- if the bullet hit any asteroid -> remove bullet
      | otherwise = False
            where
                  locationsAndRadiusList = map getBulletLocation bs
                  boolList = map (hit (Location x y) radius) locationsAndRadiusList -- check per bullet if hit the asteroid


-- | Adjusting the speed of a bullet 
bulletVelocity :: Direction -> Velocity
bulletVelocity (Vector2d dx dy) = Vector2d (dx*4.5) (dy*4.5)


-- | adjust the bullets that did NOT HIT anything
adjustBulletLocations :: [Bullet] -> [Bullet]
adjustBulletLocations = mapMaybe updateBullet

updateBullet :: Bullet -> Maybe Bullet
updateBullet b@(Bullet loc@(Location lx ly) v@(Vector2d vx vy) travalledDistance)
      | lx < -500 || lx > 500 || ly < -350 ||  ly > 350 || travalledDistance >= 100 = Nothing  -- check if the bullet has reached it maximum travel distance or if it is outside of the screen  
      | otherwise = Just (Bullet (Location newX newY) v (travalledDistance + 1))
        where
            newX = lx + vx * (mag/10)
            newY = ly + vy * (mag/10)
            mag = sqrt (vx * vx + vy * vy)


-- | Return Nothing if the bullet hit an asteroid
deleteBullet :: [Asteroid] -> Bullet -> Maybe Bullet
deleteBullet as b@(Bullet loc@(Location lx ly) velocity@(Vector2d vx vy) travelledDistance)  
      | or boolList = Nothing -- if the bullet hit any asteroid -> remove bullet
      | otherwise = Just b
            where
                  locationsAndRadiusList = map getAsteroidLocation as
                  boolList = map (hit loc 5) locationsAndRadiusList -- check per asteroid if it was hit by the bullet

partitionAsteroids :: [Bullet] -> [Asteroid] -> ([Asteroid], [Asteroid])
partitionAsteroids bs = partition (didABulletHitMe bs) 


