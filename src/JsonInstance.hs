{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module JsonInstance where

import Model
import Data.Aeson
import Data.Maybe
import Graphics.Gloss
import System.Random

import GHC.Generics

instance ToJSON World where
    -- this generates a Value
    toJSON (World player keys asteroids bullets _ score _ _ elapsedTime) =
        object ["player" .= player, "keys" .= keys, "asteroids" .= asteroids, "bullets" .= bullets, "score" .= score, "elapsedTime" .= elapsedTime]
     -- this encodes directly to a bytestring Builder
    toEncoding (World player keys asteroids bullets state score pics seed elapsedTime) =
        pairs ("player" .= player <> "keys" .= keys <> "asteroids" .= asteroids <> "bullets" .= bullets <> "score" .= score <> "elapsedTime" .= elapsedTime)

--player instances
instance ToJSON Player where
    toJSON (Player location direction velocity) =
        object ["location" .= location, "direction" .= direction, "velocity" .= velocity]
    toEncoding (Player location direction velocity) =
        pairs ("location" .= location <> "direction" .= direction <> "velocity" .= velocity)

instance ToJSON Location where
    toJSON (Location x y) =
        object ["x" .= x, "y" .= y]
    toEncoding (Location x y) =
        pairs ("x" .= x <> "y" .= y)

instance ToJSON Vector2d where
    toJSON (Vector2d xDir yDir) =
        object ["xDir" .= xDir, "yDir" .= yDir]
    toEncoding (Vector2d xDir yDir) =
        pairs ("xDir" .= xDir <> "yDir" .= yDir)

instance ToJSON Asteroid where
    toJSON (Asteroid middle radius velocityA speed) =
        object ["middle" .= middle, "radius" .= radius, "velocityA" .= velocityA, "speed" .= speed]
    toEncoding (Asteroid middle radius velocityA speed) =
        pairs ("middle" .= middle <> "radius" .= radius <> "velocityA" .= velocityA <> "speed" .= speed)

instance ToJSON Bullet where
    toJSON (Bullet locationB velocityB travalledDistance) =
        object ["locationB" .= locationB, "velocityB" .= velocityB, "travalledDistance" .= travalledDistance]
    toEncoding (Bullet locationB velocityB travalledDistance) =
        pairs ("locationB" .= locationB <> "velocityB" .= velocityB <> "travalledDistance" .= travalledDistance)



-- -- create a new world from a json object
-- instance FromJSON World where
--     parseJSON (Object v) = World <$>
--                            v .: "player" <*>
--                            v .: "keys" <*>
--                            v .: "asteroids" <*>
--                            v .: "bullets" <*>
--                            v .: "state" <*>
--                            v .: "score" <*>
--                            v .: "pics" <*>
--                            v .: "seed" <*>
--                            v .: "elapsedTime"
--     parseJSON _ = mempty
