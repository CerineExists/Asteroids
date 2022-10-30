module HelpFunctions where
import Model 
-- | remove Just from the input
noJust :: Maybe a -> a
noJust (Just a) = a
noJust Nothing = undefined --never happens

-- | Check if the input == Nothing
isItNothing :: Maybe a -> Bool
isItNothing Nothing = True
isItNothing _ = False


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


