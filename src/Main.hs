
module Main where

import Graphics.Gloss
import View
import Controller
import Model

window :: Display
window = InWindow "Asteroids" (500, 500) (0, 0)

background :: Color
background = black



main :: IO ()
main = do
    d <- loadBMP "Projectile.bmp"
    display window background d -- Step function
              
              
