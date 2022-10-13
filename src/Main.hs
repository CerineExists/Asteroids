
module Main where

import Graphics.Gloss
import View
import Controller
import Model
window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing           -- Step function
              
