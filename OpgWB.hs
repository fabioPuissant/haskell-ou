module OpgWB where
    
-- Is Even
isEven n = if m > 1 then isEven(m-2) else m==0
    where m = abs n
isEven' (n) = n > 0 && isEven' (n-2) || n == 0 

-- 1.6  Class Figure 
data Figure = Point Float Float | Circle Float Float Float | Rectangle Float Float Float Float
area :: Figure -> Float
area (Point x y) = 0.0
area (Circle x y r) = 3.14159 * r * r
area (Rectangle x y w h) = w*h


-- Opg 1.7
data IntList = Nil | Cons Int IntList deriving Show
intList1 = Nil
intList2 = Cons 13 Nil
intList3 = Cons 2 (Cons 3 (Cons 5 (Cons 7 Nil)))

empty :: IntList -> Bool
empty Nil = True
empty (Cons i l) = False

lastElem :: IntList -> Int
lastElem (Cons elem Nil) = elem
lastElem (Cons _ rest) =  lastElem rest

firstElem :: IntList -> Int
firstElem (Cons i rest) = i

sumList :: IntList -> Int
sumList Nil = 0
sumList (Cons elem rest) = elem + (sumList rest)


-- Opdracht 2.9
-- a)
twoTimes :: [Integer] -> [Integer]
twoTimes = map(2*)

twoTimes' :: [Integer] -> [Integer]
twoTimes' xs = [x*2 | x <- xs]
-- b)
largest :: [Integer] -> Integer
largest =  foldr max 0


-- Opdracht 3.3 (nested-block structure in Haskell)
nested x y = g (x+y)
    where g u = u*u

diff f = f' 
    where f' x = (f (x + h) - f x) / h
            where h = 0.001
-- Voobeeld van flat blok structure  in Haskell
addOne x = x+1
addTwo y = y+2
addThree z = addTwo(addOne z)


-- Opdracht 3.4 (toon aan dat Haskell statische scoping heeft)
s = 3
add i = i + s
m = add 10
    where s = 1

--closure in Haskell
telOp :: Int -> (Int -> Int)
telOp y = \x -> x+y     -- \x -> x+y is een lambda functie
plus_5 x = telOp 5 y
plus_4 x = telOp 4 y
plus_6 x = telOp 6 y -- de 6 wordt ingevuld voor y in de lamda, het arugment x wordt door gegeven aan de lambda

mijnFunc :: Bool -> (Int -> Int)
mijnFunc b  | b         = (telOp 1)
            | otherwise = (telOp -1)