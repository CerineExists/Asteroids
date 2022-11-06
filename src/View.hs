-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- | Tekenen
viewBMP (World (Player (Location x y) degree v) keys as bs state score (PicList r [r1, r2, r3, r4] space a ufo) _ time) 
                            | state == Playing = return $ pictures scene
                            | otherwise = return $ pictures ( scene ++ msg)      
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

                                        msg = case state of
                                                Pause -> [translate (-20) 0 $ scale 0.3 0.3 $ color white $ text "Pause"]
                                                Dead ->  [translate (-25)  0 $ scale 0.3 0.3 $ color white $ text "GAME OVER"]

                                        
-- op basis van elapsedTime één vd 2 sprites te kiezen                                                     

translateAsteroid :: Picture -> Asteroid -> Picture 
translateAsteroid pic (Asteroid (Middle x y) radius _ _) =  translate x y $ scale (0.005 * radius) (0.005 * radius) pic

translateBullets :: Bullet -> Picture
translateBullets (Bullet (Location x y) _ _) = translate x y (color green (thickCircle 3 5))

scoreText :: Int -> [Picture]
scoreText score = [translate (-490) 230 $ scale 0.1 0.1  $ color white (text ("Score: " ++ show score))]

timeText :: Float -> [Picture]
timeText t = [translate (-490) 180 $ scale 0.1 0.1  $ color white (text ("Elapsed time: " ++ show t))]

