module Herhaling where
import Hugs.Trex
signum x    | x > 0         = 1
            | x == 0        = 0
            | otherwise     = -1

power x n   | n == 0 = 1
            | otherwise = x * power x (n-1)

power' x 0 = 1
-- n+1 zegt zolang n>=1
power' x (n+1)  = x * power' x n

-- Opg 1.3
facul x | x == 1    = 1
        | otherwise = x * facul (x-1)

facul' 1 = 1
facul' (n+1) = (n+1) * facul' n
-------------
abcFormule a b c = [(-b+d)/n, (-b-d)/n]
    where   d = sqrt (b*b-4*a*c)
            n = 2*a
abcFormule' a b c = 
    let     d = sqrt (b*b-4*a*c)
            n = 2*a
    in [(-b+d)/n, (-b-d)/n]

diff f = f'
    where   f' x = (f (x+h) - f x) / h
            h = 0.0001

type Matrix a = [[a]]
data Lijst a =  Elem a (Lijst a) | Leeg deriving Show

type Name = [Char]
data Anniversary = Birthday Name Int Int Int       -- name, year, month, day
                 | Wedding String String Int Int Int -- spouse name 1, spouse name 2, year, month, day
                 deriving Show
fabioPuissant = Birthday "Fabio Noah Puissant" 1994 7 12 
wedding = Wedding "John Smith" "Jane Smith" 2022 5 18

showDate :: Int -> Int -> Int -> String
showDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name year month day) =
   name ++ " born " ++ showDate year month day
showAnniversary (Wedding name1 name2 year month day) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate year month day

concatl [] ys = ys
concatl (x:xs) ys = x : (concatl xs ys)

concatr xs [] = xs
concatr xs (y:ys) = y : (concatr xs ys)

data AorB = A Int | B Integer | C Char | F Float | S String deriving Show
type MixedList = [AorB]

getMixedListOf :: AorB -> AorB -> MixedList
getMixedListOf a b = [a,b]

hhead :: [a] -> a
hhead (x:xs) = x

ttail :: [a] -> [a]
ttail (x:xs) = xs

doThis :: (a->a) -> [a] -> [a]
doThis f [] = []
doThis f (x:xs) = f x : (doThis f xs)

minner a = -a 