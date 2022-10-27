-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- | Tekenen
viewBMP :: World -> IO Picture
viewBMP (World (Player (Location x y) degree v) keys as bullets state score) = do     
                                                            raket <- loadBMP "raketBMP.bmp"     -- self made
                                                            space <- loadBMP "space.bmp"        -- https://opengameart.org/content/space-backdrop
                                                            asteroid <- loadBMP "asteroid.bmp"  -- https://opengameart.org/content/asteroid-generator-and-a-set-of-generated-asteroids
                                                            if state == Playing 
                                                                then return $ pictures (
                                                                    [space,  
                                                                    translate x y $ rotate (90 - angle degree) raket] ++ 
                                                                    map translateBullets bullets ++
                                                                    map (translateAsteroid asteroid) as 
                                                                    ++ scoreText score )
                                                                else return $ pictures (
                                                                    [space,  
                                                                    translate (x*10) (y*10) $ rotate (90 - angle degree) raket] ++ 
                                                                    map translateBullets bullets ++
                                                                    map (translateAsteroid asteroid) as ++ 
                                                                    [translate (-180) (-35) $ color white (text "Pause")]
                                                                    ++ scoreText score) 
                                                                                                        

translateAsteroid :: Picture -> Asteroid -> Picture 
translateAsteroid pic (Asteroid (Middle x y) radius _ _) = scale (0.05 * radius) (0.05 * radius) (translate (x*100) (y*100) pic)

translateBullets :: Bullet -> Picture
translateBullets (Bullet (Location x y) _ _) = translate (x*10) (y*10) (color green (thickCircle 3 5))

--data Bullet = Bullet {locationB :: Location, velocityB :: Velocity, travalledDistance :: Float}


scoreText :: Int -> [Picture]
scoreText score = [translate (-490) 230 $ scale 0.1 0.1  $ color white (text ("Score: " ++ show score))]

