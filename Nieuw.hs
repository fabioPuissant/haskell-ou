module Nieuw where
kwadraat x=x*x
aantalOpl a b c  =  signum (b*b-4*a*c) + 1
powe x n    | n==0 = 1
            | n>0 = x * powe x (n-1)
-- patroon definitie
power x 0 = 1
power x n   = x * power x (n-1)
-- operator definitie
x ^^ 0 = 1
x ^^ (n+1) = x * x ^ n

-- naamgeven van deelformules
abcFormule' a b c = 
    [ 
        (-b+d)/n ,
        (-b-d)/n
    ]
    where 
        d = sqrt (b*b-4*a*c)
        n = 2*a
-- hetzelfde maar met let-in
abcFormule a b c = 
    let 
        d = sqrt (b*b-4*a*c)
        n = 2 * a
    in
        [(-b+d)/n , (-b-d)/n]

-- functie als parameter 
-- voorbeeld oproepen van diff: "diff kwadraat 5" diff heeft als argument een functie die zelf 1 argument heeft 
diff f = f'
    where 
        f' x    = (f (x+h) - f x) / h
        h       = 0.0001
-- lijsten                  dit geeft het type aan
intlijst = [1,2,3,4,3,8] :: [Integer]
lijst_van_lijsten_integer = [[1,2],[3,4,4]]  -- type> :: [[Integer]]
-- operator ':' Vult de lijst op de kop
example_dubbelepunt = 1:(2:(3:[]))

-- 5.3 Strings, is eigenlijk van type [char] bijgevolg kan het volgende:
hallow = "Hallo World!"
zondag = "zon" ++ "dag"
maandag = ['m','a','a','n'] ++ ['d','a','g']
dinsdag = ['d','i','n','s'] ++ 'd':'a':'g':[]
woensdag = ['w','o','e','n','s'] ++ "dag"
donderdag = "donder" ++ 'd':'a':'g':[]

-- 5.4 Tupels: groeperen van waarden met verschillende ==> mag maar moet NIET
--   tupel == record
t2 = (1, 'a')
t3 = ("aap", True, 2)
t2b = (1, 'a') :: (Integer, Char)
t3b = ("aap", True, 2) :: (String, Bool, Integer)

-- 5.5 Functies op Lijsten en Tupels
tail_lijst = tail [1,2,3] -- geeft lijst van elementen terug behalve het eerste element
length_lijst xs = length xs
is_lange_lijst xs = length xs > 10
grootte_lijst xs    | length_lijst xs < 10  = "kort" --kan ook eigen functie aanroepen!
                    | length xs <= 15       = "lang"
                    | otherwise             = "erg lang"
-- beter met interne var voor length xs
grote_lijst xs      | n < 10    = "kort"
                    | n <= 15   = "lang"
                    | otherwise = "erg lang"
            where n = length xs
-- recursief is vaak de bedoeling bij lijsten (zo is de defintie van functie 'length' lijst)
gr_lijst xs | null xs       = 0 -- null levert True als xs leeg is
            | otherwise     = 1 + gr_lijst (tail xs)
-- lijst partonen
eerste_2 (x:(y:xs)) = [x,y]
laatste_2 ((x:(y:xs)))  | n > 2 && n `mod` 2 == 0 = laatste_2 xs
                        | n == 2 = [x,y]
                        | otherwise = laatste_2 (y:xs)
            where 
                ls = ((x:(y:xs)))
                n = length ls
eerste_2' (x:xs) = [x, head xs]
laatste_2' (x:xs) | length (x:xs) == 2 = x:xs
                  | otherwise = laatste_2' xs

kwadraten (x:xs) = kwadraat x : kwadraten xs
kwadraten [] = []
ls = [1,2,3,4,5]
mapToKwadr xs = map (kwadraat) xs

-- fold(r|l) = uitvouwen, recursief toepassen van een functie
foldr_sum (x:xs) = foldr (+)  x xs
foldr_concat (x:xs) = foldr (++)  x xs -- foldR begint met uiterst rechtse element
foldl_concat (x:xs) = foldl (++)  x xs -- foldL begint met uiterst linkse element

-- Samenstellen van functies
even' x = False ||  x `mod` 2 == 0
oneven n = (not . even) n

-- 6.5 Partieel parametriseren 
plus a b = a+b
opvolger :: Integer -> Integer
opvolger = plus 1 -- is hetzelfde als 'opvolger x = plus 1 x'

-- handig bij map --<> hier de functie= ' functie die 5 ergens mee optelt, wat is nog niet gekenend'
plus_5 xs = map (plus 5) xs

square' x = x*x
squares' xs = map (square') xs
square_sum xs = foldr (+) 0 (squares' xs) 

-- currying examples
add' :: Int -> (Int -> Int)
add' = (\x -> (\y -> x+y)) -- dit is een lambda
doubleList = map (\x -> 2*x) -- sinds map al een argument van lijst heeft is dit impliciet afgeleid en dus noet te vermelden

-- 6.7 Type definities
    -- type Punt = (Float, Float)
    -- ':type Punt' => (Float, Float) // is dus geen data structuur opzich maar net een alias voor een lijst, tuple
    -- String => [Char] is een alias (afkorting) voor een lijst van Char elementen


-- 7.1 Datadeclaraties
-- Voorbeeld data declaratie:
data Boom a = Leeg                          -- Leeg en Splits zijn constuctor functies (herken ==> staten met Hoofdletter)
            | Splits a (Boom a) (Boom a)    -- hier heeft '|' de betekenis 'of'
-- Lezen als:
    -- "Een boom met elementen 'a' kan op 2 manieren opgebouwd worden: door 1) de functie Leeg te gebruiken, of 2) de functie Splits toe te passen op 3 parameters"
-- constructor functies zijn Functie waarmee de (nieuwe) datastructuur wordt opgebouwd, vaak recursief.


-- 7.2 Functies op bomen
-- vb Boom aanmaken die op p. 6 staat
-- gestructuureerde Layout
b1 = Splits 4 ( Splits 2 
                (Splits 1 Leeg Leeg )
                (Splits 3 Leeg Leeg )
         )
         ( Splits 6
                (Splits 5 Leeg Leeg)
                (Splits 7 Leeg Leeg)
         )
b2 = Splits 4 (Splits 2(Splits 1 Leeg Leeg) (Splits 3 Leeg Leeg )) (Splits 6(Splits 5 Leeg Leeg) (Splits 7 Leeg Leeg))

-- List Comprehension
example = [x^2 | x <- [1..5]]   -- geef de lijst  [1,4,9,16,25]  
                                -- x <- [..] is een generator!
example2 = [(x,y) | x <- [1..5], y <-[4,5]]  -- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5),(4,4),(4,5),(5,4),(5,5)] = KARTHESISH PRODUCT


-- 7.3 Data declaraties voor speciale types

-- Enums (opsommingstypes)      => De constructor functies dienen dan als constanten om de elementen uit de enum aan te duiden
data Color = Yellow | Red | Blue
data Richting = Noord | Oost | Zuid | West
-- Voorbeeld
stap :: Richting -> (Int, Int) -> (Int, Int)
stap Noord (x,y) = (x, y+1)
stap Oost (x,y) = (x+1, y)
stap Zuid (x,y) = (x, y-1)
stap West (x,y) = (x-1, y)

-- disjunctie vereniging van types
-- kunnen m.b.v. 'data' data types verenigen Voorbeeld: Gemengde lijst
data IntOrChar = EenInt Int | EenChar Char -- deriving (Show)  -- om auto te tonen in winhugs 'deriving (Show)'
-- Nu kan je iets doen als:
example_mixed_list :: [IntOrChar]
example_mixed_list = [EenInt 1, EenChar 'c', EenInt 3]

--custom tonen van nieuw data type
showIntOrChar :: IntOrChar -> String
showIntOrChar (EenInt a) = show a 
showIntOrChar (EenChar a) = a:[]
instance Show IntOrChar where
    show = showIntOrChar

-- Is Even
isEven n = if m > 1 then isEven(m-2) else m==0
    where m = abs n
isEven' (n) = n > 0 && isEven' (n-2) || n == 0 
