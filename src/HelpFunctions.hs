module HelpFunctions where
import Model 

-- | Find the new location of the player based on it's current location, direction and velocity
findNewLocation :: Location -> Velocity -> Location 
findNewLocation (Location x y) (Vector2d mx my) = Location newX newY
            where
                newX | x < -500 = 500 + mx 
                     | x > 500 = -500 + mx
                     | otherwise = x + mx
                newY |  y < -250 = 250 + my
                     | y > 250 = -250 + my
                     | otherwise = y + my

                     
degreeToVector :: Float -> (Float, Float)
degreeToVector degree = normalize (x, y)
                            where
                                x = cos radians
                                y = sin radians
                                radians = degree * (pi / 180)


normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) = (newX, newY)
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




