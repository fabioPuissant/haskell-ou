module Opgavenset where
-- Opgave 1 
    som :: Int -> Int -> Int
    som x y = x+y

-- Opgave 2
    -- a) gecurryde functie
    gemiddelde :: (Float ->( Float -> Float))
    gemiddelde x y = (x+y)/2

    --b) niet gecurryde functie 
    gemiddelde' :: Float -> Float
    gemiddelde' x y = (x + y) / 2