module JsonInstance where

import Model
import Text.JSON
import Data.Maybe
import Graphics.Gloss
import System.Random
instance JSON World where
  showJSON w = makeObj [("player", showJSON (player w)), 
                        ("keys", showJSON (keys w)), 
                        ("asteroids", showJSON (asteroids w)), 
                        ("bullets", showJSON (bullets w)), 
                        ("state", showJSON (state w)), 
                        ("score", showJSON (score w)), 
                        ("pics", showJSON (pics w)), 
                        ("seed", showJSON (seed w)),
                        ("elapsedTime", showJSON (elapsedTime w))]
  readJSON (JSObject obj) = do
    player <- readJSON (fromJust (lookup "player" (fromJSObject obj)))
    keys <- readJSON (fromJust (lookup "keys" (fromJSObject obj)))
    asteroids <- readJSON (fromJust (lookup "asteroids" (fromJSObject obj)))
    bullets <- readJSON (fromJust (lookup "bullets" (fromJSObject obj)))
    state <- readJSON (fromJust (lookup "state" (fromJSObject obj)))
    score <- readJSON (fromJust (lookup "score" (fromJSObject obj)))
    pics <- readJSON (fromJust (lookup "pics" (fromJSObject obj)))
    seed <- readJSON (fromJust (lookup "seed" (fromJSObject obj)))
    elapsedTime <- readJSON (fromJust (lookup "elapsedTime" (fromJSObject obj)))
    return (World player keys asteroids bullets state score pics elapsedTime seed)
  readJSON _ = error "Error while reading JSON"

-- makes an instance of the JSON class for the player
instance JSON Player where
  showJSON p = makeObj [("location", showJSON (location p)), ("direction", showJSON (direction p)), ("velocity", showJSON (velocity p))]
  readJSON (JSObject obj) = do
    location <- readJSON (fromJust (lookup "location" (fromJSObject obj)))
    direction <- readJSON (fromJust (lookup "direction" (fromJSObject obj)))
    velocity <- readJSON (fromJust (lookup "velocity" (fromJSObject obj)))
    return (Player location direction velocity)
  readJSON _ = error "Error while reading JSON"

-- makes an instance of the JSON class for the location and direction
instance JSON Location where
  showJSON (Location x y) = makeObj [("x", showJSON x), ("y", showJSON y)]
  readJSON (JSObject obj) = do
    x <- readJSON (fromJust (lookup "x" (fromJSObject obj)))
    y <- readJSON (fromJust (lookup "y" (fromJSObject obj)))
    return (Location x y)
  readJSON _ = error "Error while reading JSON"

-- makes an instance of the JSON class for the vector
instance JSON Vector2d where
  showJSON (Vector2d x y) = makeObj [("x", showJSON x), ("y", showJSON y)]
  readJSON (JSObject obj) = do
    x <- readJSON (fromJust (lookup "x" (fromJSObject obj)))
    y <- readJSON (fromJust (lookup "y" (fromJSObject obj)))
    return (Vector2d x y)
  readJSON _ = error "Error while reading JSON"

-- makes an instance of the JSON class for the asteroid {Location, Radius, Velocity, Float}
instance JSON Asteroid where
  showJSON a = makeObj [("location", showJSON (loca a)), ("radius", showJSON (radius a)), ("velocity", showJSON (veloa a)), ("speed", showJSON (speed a))]
  readJSON (JSObject obj) = do
    location <- readJSON (fromJust (lookup "location" (fromJSObject obj)))
    radius <- readJSON (fromJust (lookup "radius" (fromJSObject obj)))
    velocity <- readJSON (fromJust (lookup "velocity" (fromJSObject obj)))
    speed <- readJSON (fromJust (lookup "speed" (fromJSObject obj)))
    return (Asteroid location radius velocity speed)
  readJSON _ = error "Error while reading JSON"

loca :: Asteroid -> Location
loca (Asteroid l _ _ _) = l

veloa :: Asteroid -> Vector2d
veloa (Asteroid _ _ v _) = v

-- makes an instance of the JSON class for the bullet
instance JSON Bullet where
  showJSON b = makeObj [("location", showJSON (locationB b)), ("direction", showJSON (dirb b)), ("velocity", showJSON (velob b))]
  readJSON (JSObject obj) = do
    location <- readJSON (fromJust (lookup "location" (fromJSObject obj)))
    direction <- readJSON (fromJust (lookup "direction" (fromJSObject obj)))
    velocity <- readJSON (fromJust (lookup "velocity" (fromJSObject obj)))
    return (Bullet location direction velocity)
  readJSON _ = error "Error while reading JSON"

dirb :: Bullet -> Vector2d
dirb (Bullet _ d _) = d

velob :: Bullet -> Vector2d
velob (Bullet _ v _) = v

-- makes an instance of the JSON class for the state
instance JSON State where
  showJSON s = makeObj [("state", showJSON (s))]
  readJSON (JSObject obj) = do
    state <- fromJust (lookup "state" (fromJSObject obj))
    return state
  readJSON _ = error "Error while reading JSON"

-- makes an instance of the JSON class for the PicList {lazyRocket :: Picture, runningRockets :: [Picture], space :: Picture, asteroid :: Picture, ufo :: Picture}
instance JSON PicList where
  showJSON p = makeObj [("lazyRocket", showJSON (lazyRocket p)), ("runningRockets", showJSON (runningRockets p)), ("space", showJSON (space p)), ("asteroid", showJSON (asteroid p)), ("ufo", showJSON (ufo p))]
  readJSON (JSObject obj) = do
    lazyRocket <- readJSON (fromJust (lookup "lazyRocket" (fromJSObject obj)))
    runningRockets <- readJSON (fromJust (lookup "runningRockets" (fromJSObject obj)))
    space <- readJSON (fromJust (lookup "space" (fromJSObject obj)))
    asteroid <- readJSON (fromJust (lookup "asteroid" (fromJSObject obj)))
    ufo <- readJSON (fromJust (lookup "ufo" (fromJSObject obj)))
    return (PicList lazyRocket runningRockets space asteroid ufo)
  readJSON _ = error "Error while reading JSON"

-- makes an instance of the JSON class for the Picture
instance JSON Picture where
  showJSON p = makeObj [("picture", showJSON (p))]
  readJSON (JSObject obj) = do
    picture <- readJSON (fromJust (lookup "picture" (fromJSObject obj)))
    return picture
  readJSON _ = error "Error while reading JSON"

-- makes an instance of the JSON class for the stdGen
instance JSON StdGen where
  showJSON s = makeObj [("stdGen", showJSON (s))]
  readJSON (JSObject obj) = do
    stdGen <- readJSON (fromJust (lookup "stdGen" (fromJSObject obj)))
    return stdGen
  readJSON _ = error "Error while reading JSON"