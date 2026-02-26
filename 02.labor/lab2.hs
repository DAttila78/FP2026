szjszorzat 0 = 1
szjszorzat n  = (mod n 10 ) * szjszorzat(div n 10)

szjszorzat2  n 
    | n<0 = szjszorzat2(abs n )
    | div n 10 == 0 = mod n 10*szjszorzat2(div n 10 )\
    |otherwise = mod n 10 * szjszorzat2(div n 10 )