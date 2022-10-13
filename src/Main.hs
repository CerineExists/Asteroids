
module Main where

import Graphics.Gloss
import View
import Controller
import Model
window :: Display
window = InWindow "Asteroids" (500, 500) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 4

main :: IO ()
main = display window background drawing           -- Step function
              
