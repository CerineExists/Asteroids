module HelpFunctions where
import Model 

-- | Find the new location of the player based on it's current location, direction and velocity
findNewLocation :: Location -> Velocity -> Location 
findNewLocation (Location x y) (Vector2d mx my) = Location newX newY
            where
                newX | x < -505 = 500 + mx 
                     | x > 505 = -500 + mx
                     | otherwise = x + mx
                newY |  y < -255 = 250 + my
                     | y > 255 = -250 + my
                     | otherwise = y + my

isThereAnActiveUFO :: [UFO] -> Maybe UFO
isThereAnActiveUFO [] = Nothing
isThereAnActiveUFO (x@UFO{stateUFO = s}:xs)   | s == Attacking = Just x
                                              | otherwise = isThereAnActiveUFO xs


-- | Adjusting the speed of a bullet 
bulletVelocity :: Direction -> Velocity
bulletVelocity (Vector2d dx dy) = Vector2d (dx*15) (dy*15)

mkVector :: Location -> Location -> Vector2d
mkVector (Location x1 y1) (Location x2 y2) =  normalize (Vector2d (x2 - x1) (y2 - y1))

degreeToVector :: Float -> Vector2d
degreeToVector degree = normalize (Vector2d x y)
                            where
                                x = cos radians
                                y = sin radians
                                radians = degree * (pi / 180)


normalize :: Vector2d -> Vector2d
normalize (Vector2d x y) = Vector2d newX newY
                        where
                            newX = x * multiplicationFactor
                            newY = y * multiplicationFactor
                            lengthVector = sqrt $ x * x + y * y 
                            multiplicationFactor = 1 / lengthVector


-- get the location of an asteroid
getAsteroidLocation :: Asteroid -> (Location, Radius)
getAsteroidLocation (Asteroid (Middle x y) radius _ _) = (Location x y, radius)


-- get the location of a bullet
getBulletLocation :: Bullet -> (Location, Radius)
getBulletLocation (Bullet (Location x y) _ _) = (Location x y, 5)




