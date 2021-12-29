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
