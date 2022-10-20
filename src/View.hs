-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- | Tekenen

view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture --teken ruimteschip IS NU NOG EEN CIRKEL
viewPure (World (Player (Location x y) d v ) keys) = pictures [space, translate (x*10) (y*10) raket]
        where
            raket = color white (thickCircle 3 30)
            space = color roze $ polygon (rectanglePath 1000 500) 
            roze = makeColor 0.9648 0.1055 0.7929 0.5  

viewBMP :: World -> IO Picture
viewBMP (World (Player (Location x y) d v) keys) = do     raket <- loadBMP "raketBMP.bmp"
                                                          space <- loadBMP "space.bmp"
                                                          return $ pictures [space, translate (x*10) (y*10) $ rotate (angle d) raket]
                                                    


