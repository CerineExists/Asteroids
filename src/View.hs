-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- | Tekenen
viewBMP :: World -> IO World
viewBMP (World (Player (Location x y) degree v) keys as) = do     
                                                            raket <- loadBMP "raketBMP.bmp"     -- self made
                                                            space <- loadBMP "space.bmp"        -- https://opengameart.org/content/space-backdrop
                                                            asteroid <- loadBMP "asteroid.bmp"  -- https://opengameart.org/content/asteroid-generator-and-a-set-of-generated-asteroids
                                                            return $ pictures (
                                                                [space,  translate (x*10) (y*10) $ rotate degree raket] ++
                                                                map (translateAsteroid asteroid) as)


                                                    

translateAsteroid :: Picture -> Asteroid -> Picture 
translateAsteroid pic (Asteroid (Middle x y) radius _ _) = scale (0.05 * radius) (0.05 * radius) (translate (x*100) (y*100) pic)




