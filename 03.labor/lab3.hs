import System.Directory.Internal.Prelude (modifyIOError)
import Foreign.C (errnoToIOError)
mylength [] = 0 
mylength (x:xs) = 1+ mylength xs 


mylength2 (x : xs ) res = mylength2 xs (res +1) 

mylength3 ls = foldr (\x -> (+) 1  ) 0 ls

mylength4 ls = foldr (\db x -> db+1) 0 ls 

mylength5 ls res = foldr(\x res  -> 1 +res) res ls 


myproduct [] = 1 
myproduct ls = x * myproduct xs
    where 
        (x : xs) = ls 
myproduct2(x : xs ) res = myproduct2 xs(res + x)
myproduct3 ls = foldr(\x -> (*) x ) 1 ls 

myproduct4 ls res = foldr(\x res -> (*) x res ) res ls 

myproduct5 ls = foldr1 (\x -> (*) x) ls 

myminimum (x1:x2:xs) = if x1 < x2 then myminimum (x1 : xs) else myminimum (x2 : xs)

myminimum2 (x1:x2:xs) 
    |x1 < x2 = myminimum2 (x1:xs)
    |otherwise = myminimum2(x2:xs)

myminimum3 ls = foldr1 min ls 

mymaximum (x1:x2:xs) = if x1 > x2 then mymaximum(x1:xs) else mymaximum (x2:xs)

mymaximum2 ls = foldr1 (\x1 x2 -> if x1 > x2 then x1 else x2) ls 

mymaximum3  ls = maximum ls 

listaN ls n = ls !! n 
listaN2 ls n 
    | ls == [] = error "ures lista" 
    | n < 0 = error "neg, index"
    |length ls >= n = error " tul nagy index"
    |otherwise = ls !! n 


listaFuz ls1 ls2 = ls1 + ls2

palindrom ls 
    | ls = reverse ls = "palindrom"
    | otherwise = " nem palindrom"


szjls x 
    | x < 0 = szjls (abs x )
    | x < 10 = [x]
    |otherwise = szjls (div x 10) ++ [mod x 10 ]


elsoUtolso2 ls = tail ls ++ [head ls]

decP x p 
    |x<p = [x]
    | otherwise = decP ( div x p ) ++ [mod x p]