-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Int -> Int -> Int
osszeg a b = a + b

kulonbseg :: Double -> Double -> Double
kulonbseg a b = a - b

szorzat :: Int -> Int -> Int
szorzat a b = a * b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = a `div` b

osztmar a b = mod a b

osztmar2 a b = a `mod` b

-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> a,b -> x = (-b) / a
elsoF a b = (-b) / a

-- - egy szám abszulút értékét,
abszolut a
  | a < 0 = -a
  | otherwise = a

abszolut2 a = if a < 0 then -a else a

-- - egy szám előjelét,
elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"

elojel2 n
  | n < 0 = "negativ"
  | n > 0 = "pozitiv"
  | otherwise = "nulla"

-- - két argumentuma közül a maximumot,
max_ a b = if a > b then a else b

max1 a b
  | a > b = a
  | otherwise = b

-- - két argumentuma közül a minimumot,
min_ a b
  | a < b = a
  | otherwise = b

-- - egy másodfokú egyenlet gyökeit,

masodf a b c = if delta <0 then error "komplex szamok" else (gy1, gy2)
where
  delta = 4*a*c
  gy1=(-b +sqrt delta) / (2*a)
  gy2= (-b - sqrt delta) /(2*a)


masodf2 a b c 
  | delta <0 = error "komplex"
  |delta == 0 = [gy1]
  |otherwise = [gy1, gy2]



-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.

elempar ep1 ep2 = if(a == d && b == c ) || (a== c && b == d ) then True else False
  where
    (a,b) = ep1
    (c,d) = ep2

elempar2 (a,b) (c,d) = (a == c && b == d ) || (a == d && b ==c)






-- - az n szám faktoriálisát (3 módszer),

fakt1 0 = 1 
fakt1 n = n*fakt(n-1)

fakt2 n 
  | n < 0 = error "neg szam"
  | n == 0 =1
  |otherwise =  n*fakt2(n-1)



  fakt3 n res 
    | n < 0 = error "neg szam"
    | n == 0 = res 
    |otherwise = fakt3 (n-1) (res*n)gh  


-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

hatvany x n 
  | n < 0 = error "neg kitevo"
  |otherwise = x ** n 

hatvany2  :: (Num a , Integral b ) => a -> b ->a
hatvany2 x n 
  | n < 0 = error "neg kitevo"
  |otherwise = x ** n 




-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
-- - az első n négyzetszámot,


negyzetszam n = [i ^ 2 | i<-[0..n]]



-- - az első n természetes szám köbét,

kobszam n = [i ** 3 | i<-[0..n]]


-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,

nemNegyzet n = [i | i <- [1..n] , i/=(sqrt i  ** 2)]



-- - x hatványait adott n-ig,

hatvanyx x n = [x^i | i<-[1..n]]


-- - egy szám páros osztóinak listáját,

parosOsztok x = [i | i<-[1..x], mod x i ==0, mod i 2 == 0]

-- - n-ig a prímszámok listáját,

osztok x =[i | i<- [1..x], mod x i ==0 ]

primszam = osztok x == [1,x]

primszamN  n = [i | <- [1..n], primszam i ]
primszamN n = [i | i<- [1..n],primszamL i ]
  where
    primszamL si = osztokL si == [1,si]
    osztokL si2 = [i|i<-[1..si2],mod si2 i == 0]


-- - n-ig az összetett számok listáját,

oszetettszamok n  = [i | i<-[i..n],primszam i == False]



-- - n-ig a páratlan összetett számok listáját,


-- - az n-nél kisebb Pitágorászi számhármasokat,

pitagorasz n = [(a , b , c ) | c<- [1..n],b<-[1..c],a<-[1..b],a^2 + b^2 == c^2]



-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,

betuszam =zip ['a' .. 'z' [0..25]]

-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.


main :: IO()
main = do 
  putStrLn"Masodfoku egyenlet "
  print (masodf 1 2 1)
  putStrLn ("Masodfoku ")
  putStrLn("Elemparok:  ")
  print(elempar (6,7)  (7,6) )
  putStrLn("Faktorialis : ")
  print (fakt (-5))
  putStrLn("Hatvany")
  print(hatvany2 2 4 )
  putStrLn("Kobszam" ++ show(kobszam 11))