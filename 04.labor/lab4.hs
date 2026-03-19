-- # 4. labor


-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:
-- - az első n páros szám négyzetét,
-- - az első [1, 2, 2, 3, 3, 3, 4, 4, 4, 4,...],

szamokLs n 
    | n/=0 = szamokLs (n-1) ++  replicate n n 
    |otherwise = replicate n n 

szamokLs2 n i 
    | i/=n = replicate i i : szamokLs2 n (i+1)
    |otherwise = replicate i i  



-- - az első [2, 4, 4, 6, 6, 6, 8, 8, 8, 8...],

szamokLs3 n i 
    | i/=n = replicate i (i*2) : szamokLs3 n (i+1)
    |otherwise = replicate i (i*2) 

-- - az első [n, n-1, ..., 2, 1, 1, 2, ..., n-1, n],

szamokLs4 n = [n,n-1 .. 1 ] ++ [1..n]
szamokLs5 n = reverse [1 .. n]  ++ [1..n] 

-- - váltakozva tartalmazzon True és False értékeket,

tfLs n  = take n ls 
    where
        ls = [True, False] ++ ls 

-- - váltakozva tartalmazza a 0, 1, -1 értékeket.




-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely


-- - meghatározza egy adott szám osztóinak számát,

osztok n = [i | i <- [1.. n ] , mod n i ==  0 ]

osztokSzama n = length $ osztok n 

--  fold1 -> foldl
osztokSzama3 n = foldl (\res i -> if mod n i == 0 then res+1 else res) 0 [1..n]

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,

--  hiányzó $ az osztok n elé
maxParatlanOSzto n  = last $ filter odd $ osztok n

--  hiányzó 'i' változónév a generátorban
maxParatlanOszto2 n = [i | i <- [1,3 .. n ], mod n i == 0 ]

--  fold1 -> foldl, és szóköz a lambda elé
maxParatlanOSzto3 n = foldl (\res i -> if mod n i == 0 then i else res) 1 [1,3 .. n]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,

decPszam x p = length $ decP x p 

decPMax x p = maximum $ decP x p 


-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,

decPMax2 x p = myMaximum $ decP x p 
    where
        myMaximum [n ] = n 
        myMaximum (n1 : n2 : ls )
            |n1 > n2 = myMaximum (n1 : ls)
            |otherwise = myMaximum (n2 : ls )

-- - meghatározza az a és b közötti Fibonacci számokat, a > 50.

--  helyes Fibonacci rekurzió
fibo = 0 : 1 : zipWith (+) fibo (tail fibo)

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely


-- - meghatározza egy lista pozitív elemeinek átlagát,

atlag ls = sum ls / fromIntegral (length ls )

--  1 > 0 helyett i > 0
pozAtlag ls = atlag [i | i <- ls , i > 0]

--  'pozAtlag 2' -> 'pozAtlag2'
pozAtlag2 ls = (atlag . filter (>0)) ls 

-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,

listaN ls n = [i | (idx, i) <- zip [1..] ls , mod idx n == 0 ]

--hiányzó feltétel (kilépési eset) és hiányzó jobb oldal az első őrben
listaN2 ls n i 
    | length ls <= i = []
    | mod i n == 0   = ls !! i : listaN2 ls n (i+1) 
    | otherwise      = listaN2 ls n (i+1)

-- - tükrözi egy lista elemeit,
-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.