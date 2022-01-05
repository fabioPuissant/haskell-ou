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