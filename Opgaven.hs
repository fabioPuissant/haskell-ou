module Opgaven where 
-- Opgave 1.1
derde_macht x = x * x * x

-- Opgave 1.2
aantalOpl a b c = signum(b*b-4*a*c) + 1

-- Opgave 1.3
    -- a) facul dmv voorwaarden
faculV x    | x==0 = 1
            | x > 0 = x * faculV (x-1)
    -- b) facul dmv patronen
faculP 0 = 1
faculP x = x * faculP (x-1)

-- Opgave 1.4 is niet in Haskell
-- Opgave 1.5: Ga na (met de interpretor) wat de operator '++' doet (lijsten), 
-- wat is het verschil met de operator ':'
operator_plusplus = [1,2,3] ++ [3,4,5,1]    -- concat 2e lijst achter 1e lijst en geeft het resultaat terug
--operator_colon = [1,2,3] : [3,4,5,1]      -- geeft error, kan maar 1 element/keer aan de kop toevoegen

-- Opgave 1.7
-- a) recursief gevalonderscheid met voorwaarden
recvor_eenVanDe xs  | null xs       = False
                    | otherwise = head xs || recvor_eenVanDe (tail xs)
-- b) recursief gevalonderscheid met patronen
recpat_eenVanDe [] = False
recpat_eenVanDe (x:xs) =  x || recpat_eenVanDe xs
-- c) met foldr
foldr_eenVanDe xs = foldr (||) False xs

-- Opgave 1.8 Bepaal type van functie 'map' m.b.v. interpreter 
    -- commando:    ':type map'  
    -- result:      'map :: (a->b) -> [a] -> [b]'

-- Opgave 1.9
-- a)        div   :: Integral => a -> a -> a -> a
--          quot  :: Integral => a -> a -> a -> a
--      in beide gevallen moet het type van 'a' afgeleid zijn van het type 'Integral'
--      de functies 'div' en 'quot' zijn dus overloaded voor de typen uit deze klasse ("Integral")
-- b) Integral a element_of {Int, Integer} ==> Int en Integer mogen NIET door elkaar gebruikt worden!!
-- c) Het type van 7.3 is Fractional

-- Opgave 1.10
som xs = foldr (+) 0 xs
som' = foldr (+) 0

-- Opgave 1.11
even'' x = x `mod` 2 == 0 
oneven = not . even 

-- Opgave 1.12
kwadraat x = x*x
kwadr_1_to_10 = map (kwadraat) [1,2,3,4,5,6,7,8,9,10]

-- Opgave 1.12 
--  a)
    -- '(plus 3) (plus 4 5)' de haakjes bij de 1e plus zijn overbodig want functie beschrijving is links associatief 
    -- dus 'plus 3 (plus 4 5)' gaat ook
--  b) 
    -- beide paren haakjes zijn overbodig 
    -- DUS 'sqrt (3) + (sqrt 4)' kan worden 'sqrt 3 + sqrt 4'
--  c)
    -- Het 2e paar haakjes is overbodig, want '->' expressies zijn rechts associatief
    -- DUS '(a->b) -> (c->d)' kan worden 'a-> b -> c -> d'