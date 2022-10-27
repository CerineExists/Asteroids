module HelpFunctions where

-- | remove Just from the input
noJust :: Maybe a -> a
noJust (Just a) = a
noJust Nothing = undefined --never happens

-- | Check if the input == Nothing
isItNothing :: Maybe a -> Bool
isItNothing Nothing = True
isItNothing _ = False

