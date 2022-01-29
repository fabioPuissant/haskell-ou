module OpgWB where
    
-- Is Even
isEven n = if m > 1 then isEven(m-2) else m==0
    where m = abs n
isEven' (n) = n > 0 && isEven' (n-2) || n == 0 

isEven'' :: Int -> Bool
isEven'' n  | n < 0 = isEven'' (-n)
            | n == 1 = False
            | n == 0 = True
            | otherwise = isEven'' (n-2)

is_Even :: Int -> Bool
is_Even 0 = True
is_Even 1 = False
is_Even (n+2) = is_Even n
is_Even n = is_Even (-n)


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
telOp x = \y -> x+y     -- \y -> x+y is een lambda functie, deze functie pakt x aan en geeft een andere functie terug die weer een parameter neemt
plus_5 y = telOp 5 y
plus_4 y = telOp 4 y
plus_6 y = telOp 6 y -- de 6 wordt ingevuld voor x in de lamda, het arugment y wordt door gegeven aan de lambda

makeT :: a -> b -> (a,b)
makeT x = \y -> (x,y)
makeT2 s = makeT s 2

mijnFunc :: Bool -> (Int -> Int)
mijnFunc b  | b         = (telOp 1)
            | otherwise = (telOp (-1))
-- Gebruik van mijnFun
    -- (mijnFunc True) 2 // het geen tussen haakjes returns een functie telOp 1 y, deze y waarde wordt nu gevuld met 2
    -- (mijnFunc False) 2 // het geen tussen haakjes returns een functie telOp -1 y, deze y waarde wordt nu gevuld met 2 

timesTwo x = 2*x
fp f xs = map (f) xs

-- Opg 1.9
type Author = (String, Int)
getAuth :: Author
getAuth = ("David Watt", 58)

type Book = (String, Int)
getBook :: Book
getBook = ("Concepts of programming languages", 2004)

hf :: Author -> String
hf (x, y)  = x

data Lijst a = Leeg | Const a (Lijst a) deriving Show
g = Const 5 Leeg

neem :: Num a => Int -> [a] -> [a]
neem n xs   | n == 0 = []
            | null xs = []
            | otherwise = (head xs) : (neem (n-1) (tail xs))


--neem' :: Int -> [a] -> [a]
----neem' 0 (x:xs) = []
--neem' n [] = []
--neem' (n+1) (x:xs) = x : neem' n xs

langer' :: [a] -> [b] -> Bool
langer' xs ys  | null xs = False
               | null ys = True
               | otherwise = langer' (tail xs) (tail ys)

langer'' :: [a] -> [b] -> Bool
langer'' [] _ = False
langer'' _ [] = True
langer'' (x:xs) (y:ys)= langer'' xs ys

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n] , (mod n x) == 0 ]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1 .. n], (isPrime x)]

tafels :: Int -> Int -> [(Int, Int, Int)]
tafels n max = [ (x, n, (x*n)) | x <- [0..max]]

tafels10 :: Int -> [(Int, Int, Int)]
tafels10 n = (tafels n 10)

keerOm :: [a] -> [a]
keerOm = foldl (\arr elem -> elem:arr) []

test :: (a-> Bool) -> [a] -> [a] 
test f = foldr (\ elem arr -> if (f elem) then elem:arr else arr) []