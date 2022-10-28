-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- | Tekenen
viewBMP :: World -> IO Picture
viewBMP (World (Player (Location x y) degree v) keys as bs state score (PicList r [r1, r2, r3, r4] space a) _ time _) 
                            | state == Playing = return $ pictures scene
                            | otherwise = return $ pictures ( scene ++   [translate (-180) (-35) $ color white (text "Pause")]  )      
                                where
                                        scene = [space,  
                                                rocket] ++
                                                bullets ++
                                                asteroids ++
                                                scoreText score ++ timeText time
                                        rocket  = translate x y $ rotate (90 - angle degree) whichRocket
                                        bullets = map translateBullets bs
                                        asteroids = map (translateAsteroid a) as 
                                        whichRocket     | 'w' `notElem` keys = r
                                                        | rest == 0 =  r1
                                                        | rest == 1 = r2
                                                        | rest == 2 = r3
                                                        | otherwise = r4 
                                        rest = round (time*24) `mod` 4
                                        
-- op basis van elapsedTime één vd 2 sprites te kiezen                                                     

translateAsteroid :: Picture -> Asteroid -> Picture 
translateAsteroid pic (Asteroid (Middle x y) radius _ _) = scale (0.005 * radius) (0.005 * radius) (translate (x) (y) pic)

translateBullets :: Bullet -> Picture
translateBullets (Bullet (Location x y) _ _) = translate x y (color green (thickCircle 3 5))

--data Bullet = Bullet {locationB :: Location, velocityB :: Velocity, travalledDistance :: Float}

scoreText :: Int -> [Picture]
scoreText score = [translate (-490) 230 $ scale 0.1 0.1  $ color white (text ("Score: " ++ show score))]

timeText :: Float -> [Picture]
timeText t = [translate (-490) 180 $ scale 0.1 0.1  $ color white (text ("Elapsed time: " ++ show t))]

