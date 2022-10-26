-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List (findIndex, elemIndex)
import Debug.Trace (trace)
import Data.Foldable
import GHC.IO.Device (IODeviceType(Directory))




-- | Handle user input
input :: Event -> World -> IO World
input e wrld = return (inputKey e w)
    where w = {-trace (show wrld)-} wrld


bulletVelocity :: Direction -> Velocity
bulletVelocity (Vector2d dx dy) = Vector2d (dx*3) (dy*3)

inputKey :: Event -> World -> World
inputKey (EventKey (SpecialKey KeySpace) Down _ _) w@(World (Player l d v) keys as bullets)  = w {bullets = Bullet l (bulletVelocity d) 0 : bullets} -- SHOOT (klopt het?)
inputKey (EventKey (Char c) Down _ _) w@(World (Player l d v ) keys as _)  = w {keys = c : keys}
inputKey (EventKey (Char c) Up _ _)   w@(World (Player l d v ) keys as _)  = w {keys = pop c keys}
inputKey _ w = w
--data Bullet = Bullet {locationB :: Location, velocityB :: Velocity}

-- | Calculate the new direction and location of the player
pop ::  Eq a =>  a -> [a] -> [a]
pop e xs = case elemIndex e xs of
  Nothing -> undefined --never happens
  Just n -> take n xs ++ drop (n+1) xs

findNewLocation :: Location -> Direction -> Velocity -> Location -- location direction velocity
findNewLocation (Location x y) (Vector2d vx vy) (Vector2d mx my) = Location newX newY
            where
                newX | x < -50 = 50 + mx 
                     | x > 50 = -50 + mx
                     | otherwise = x + mx
                newY |  y < -25 = 25 + my
                     | y > 25 = -25 + my
                     | otherwise = y + my





-- | Calculate the movements of the asteroids
adjustAsteroidList :: World -> [Asteroid] -> [Asteroid] -- return the new list of asteroids
adjustAsteroidList _ [] = []
adjustAsteroidList world (x:xs)     | isItNothing (flyingAsteroids world x) = adjustAsteroidList world xs -- delete an asteroid from the list when it is outside of the screen
                                    | otherwise = noJust (flyingAsteroids world x) : adjustAsteroidList world xs

noJust :: Maybe a -> a
noJust (Just a) = a
noJust Nothing = undefined --never happens

isItNothing :: Maybe a -> Bool
isItNothing Nothing = True
isItNothing _ = False

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

isHit :: Asteroid -> [Bullet] -> Bool
isHit _ [] = False
isHit a (Bullet loc _ _: bs)    | collision a loc = True
                                | otherwise = isHit a bs



collision :: Asteroid -> Location ->  Bool
collision a@(Asteroid m@(Middle x1 y1) radius _ _) (Location x2 y2)     | outsideSquare m radius x2 y2 = False
                                                                        | otherwise = True


outsideSquare :: Middle -> Float -> Float -> Float -> Bool
outsideSquare (Middle x1 y1) r x2 y2    | x2 < (x1 + r) && x2 > (x1 - r) &&  y2 < (y1 + r) && y2 > (y1 - r) = False -- de case
                                        | otherwise = True



-- | BULLETS

-- adjust every bullet in the list
adjustBulletList ::  World -> [Bullet] -> [Bullet]
adjustBulletList _ [] = []
adjustBulletList world (x:xs)   | isItNothing (flyingBullet world x) = adjustBulletList world xs -- delete an asteroid from the list when it is outside of the screen
                                | otherwise = noJust (flyingBullet world x) : adjustBulletList world xs

-- check per bullet what it's new location is and delete if hit something 
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


hit :: Location -> [Asteroid] ->  Bool -- Location = location of the bullet
hit _ [] = False
hit (Location x2 y2) (a@(Asteroid (Middle x1 y1) r _ _):as)     
                                                | x2 < (x1 + r) && x2 > (x1 - r) &&  y2 < (y1 + r) && y2 > (y1 - r) = True
                                                | otherwise = False




-- degreeToVector :: Float -> (Float, Float)
-- degreeToVector degree = normalize (x, y)
--                             where
--                                 x = sin radians
--                                 y = cos radians
--                                 radians = degree * (pi / 180)


normalize :: Vector2d -> Vector2d
normalize (Vector2d x y) = Vector2d newX newY
                        where
                            newX = x * multiplicationFactor
                            newY = y * multiplicationFactor
                            lengthVector = sqrt $ x * x + y * y
                            multiplicationFactor = 1 / lengthVector
v1 = Vector2d 1 1
v2 = Vector2d 1 (-1)

turn :: Vector2d -> Float -> Vector2d
v@(Vector2d x y) `turn` f = Vector2d newX newY where
    mag = sqrt(x*x + y*y)
    ang = angle v
    newX = mag * cos(pi/180 * (ang + f))
    newY = mag * sin(pi/180 * (ang + f))


stepForward :: World -> World
stepForward w@(World (Player l d v) k a _) = w {player = Player (findNewLocation l d v) d v }

stepLeft :: World -> World
stepLeft w@(World (Player l d v) k a _) = w {player = Player l  (d `turn` 1) v}

stepRight:: World -> World
stepRight w@(World (Player l d v) k a _) = w {player = Player l (d `turn` (-1)) v}


step :: Float -> World -> IO World
step _ w@(World (Player (Location x y) (Vector2d vx vy) (Vector2d mx my)) keys as _) = do -- todo change momentum
    return $ b' $ a' $ g $ foldr f w keys
    where
        f ::  Char -> World -> World
        f 'w' = stepForward
        f 'a' = stepLeft
        f 'd' = stepRight
        f  _  = id

        g :: World -> World
        g w@(World (Player (Location x y) (Vector2d vx vy) (Vector2d mx my)) k as _) = 
            w {player = Player (Location (x+mx) (y+my)) (Vector2d vx vy) (Vector2d (clamp (-0.1) 0.1 (mx+vx)) (clamp (-0.1) 0.1 (my+vy)))}

        a' :: World -> World
        a' w@(World _ _ as _) = 
            w { asteroids = adjustAsteroidList w as}

        b' :: World -> World
        b' w@(World _ _ _ bullets) = 
            w { bullets = adjustBulletList w bullets}

-- this func makes sure a value is between a min and max value
clamp :: Float -> Float -> Float -> Float
clamp min' max' val = max min' (min max' val) 

