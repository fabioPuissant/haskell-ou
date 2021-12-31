module OpgWB where
    
-- Is Even
isEven n = if m > 1 then isEven(m-2) else m==0
    where m = abs n
isEven' (n) = n > 0 && isEven' (n-2) || n == 0 

-- 1.6  Class Figure 
data Figure = Circle Float | Rectangle Float Float
area :: Figure -> Float
area Circle r = (pi * (kwadraat r))
area (Rectangle w h) = w*h