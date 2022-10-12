{-
    
 MODEL   -

blabla 

import module where...



package.haskell.org : collects modules die mensen hebben geschreven.

your-project.cabal
- metainfo
- author




module Main where
import M.A
import M.B

main :: IO ()
main = -- start running here


CABAL
$ cabal update (have the latest update of gloss)
$ cabal build
$ cabal run mijnprogramma.hs


second build tool: $ stack build or $...



MODEL:
data Model = ...
must have all the data of your program
    - where is your player
    - how many enemies are there
    - where are the enemies
    - etc. 


VIEW:
view :: Model -> Picture
how to display the model


CONTROLLER: (business logic, how to modify the Model
update :: Input -> Model -> Model
- when you get hit, your health decreases






DESIGNING THE MODEL
1. MAKE IMPOSSIBLE STATES IMPOSSIBLE TO REPRESENT
        Make sure your datatypes cannot encode the situations
        Bijv:
        type Boolean = Int (0 is False, 1 is True) DON'T DO THIS. BETTER: data Boolean = False | True
2. USE ONE TYPE PER CONCEPT
        Bijv:
        Do NOT do:
        data Object = Ship ...
                    | Player {score :: Int ..., ...}
                    | Enemy | Wall | Empty
        BETTER:
        data Ship = ...
        data Player = ...
        data Enemy = ...
        data Status = Alive | Dead (instead of using Bool)
        data Level = Finished | InProgress (instead of using Bool)
        handiger: computeScore :: Status -> Level -> Int (ipv computeScore :: Bool -> Bool -> Int)



3. ABSTRACT USING MODULES AND TYPECLASSES
        When there's common functionality, use typeclasses
            - Exporting Data types
                module Name (..., Type(...), ... )
            -  Introduce type classes
                class Positioned a where
                    getPosition :: a -> Point
                    move :: a -> Vector -> a
                types that can be rendered on the screen
                class Renderable a where
                    render :: a -> Picture


data Card = Card {value :: CardValue, 
                    suit :: Suit}

data Card = Card CardValue Suit
data Point = Pt Float Float

data CardValue = Int


class MyEq q where
    isTheSameAs :: a -> a -> Bool

instance MyEq a => MyEq (Foo a) where
    isTheSameAs _ _ = undefined


data Model = Model {  words :: [String, Int]
                    , currentTime :: Float
                    , isTyping :: Int
                    } deriving (Show, Eq)




-- Guidelines GAME

1. Scheid je pure en onpure code!!
    vaak: 
        1. Impure part which obtains the input
        2. Pure part which manipulates the data
        3. Impure part which communicates the result

For the randomness something needs to happen in the IO part        















-}



import Data.List
import Data.List (nub, permutations)
import Data.List hiding (nub)
import qualified Data.List as L
--import every function from Data.List, but you can call it L :)
-- bijv. L.nub, L.permutations


data IsTyping = IsTyping | NotTyping





