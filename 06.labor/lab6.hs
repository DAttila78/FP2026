import Data.Char
import Data.Foldable (maximumBy)
import Data.List
import Data.Ord (comparing)
import Numeric

-- # 6. labor

-- I. Írjunk egy-egy Haskell függvényt, amely beolvass a billentyűzetről két természetes számot és kiírja a képernyőre

-- - a két szám közötti számok összegét,ss
szamokOsszeg = do
  putStr "Elso szam: "
  x1 <- getLine
  putStr "Masodik szam: "
  x2 <- getLine
  let osszeg = sum [read x1 :: Int .. read x2 :: Int]
  putStrLn ("Osszeg: " ++ show osszeg)

szamokOsszeg2 = do
  putStr "Elso szam: "
  x1 <- readLn :: IO Int
  putStr "Masodik szam: "
  x2 <- readLn :: IO Int
  let osszeg = sum [x1 .. x2]
  putStrLn ("Osszeg: " ++ show osszeg)

szamokOsszeg3 = do
  putStr "Elso szam: "
  x1 <- readLn :: IO Int
  putStr "Masodik szam: "
  x2 <- readLn :: IO Int
  let ls = if x1 < x2 then [x1 .. x2] else [x2 .. x1]
      osszeg = sum ls
      reszOsszeg = scanl1 (+) ls
  putStrLn ("Osszeg: " ++ show osszeg)
  putStrLn ("Reszosszeg: " ++ show reszOsszeg)

-- - a két szám közötti prímszámok összegét,
osztok :: (Integral a) => a -> [a]
osztok x = [i | i <- [1 .. x], mod x i == 0]

primszam :: (Integral a) => a -> Bool
primszam x = osztok x == [1, x]

primszamok :: (Integral a) => a -> a -> [a]
primszamok a b
  | a < b = [i | i <- [a .. b], primszam i]
  | otherwise = [i | i <- [b .. a], primszam i]

primszamokOsszeg = do
  putStr "Elso szam: "
  x1 <- readLn :: IO Int
  putStr "Masodik szam: "
  x2 <- readLn :: IO Int
  putStrLn ("Prim osszeg: " ++ show (sum $ primszamok x1 x2))

primszamokOsszeg2 = do
  putStr "Elso szam: "
  x1 <- readLn :: IO Int
  putStr "Masodik szam: "
  x2 <- readLn :: IO Int
  let primek = primszamok x1 x2
      primOsszeg = sum primek
      reszPrimOsszeg = scanl1 (+) primek
  putStrLn ("Prim osszeg: " ++ show primOsszeg)
  putStrLn ("Resz prim osszeg: " ++ show reszPrimOsszeg)

-- - a két szám közötti azon számokat, amelyeknek legtöbb valódi osztója van.
valodiOsztok x = [i | i <- [2 .. x - 1], mod x i == 0]

valodiOsztokSzama x = length $ valodiOsztok x

legtobbValodiOsztoSzam a b = filter (\(szam, oszto) -> oszto == maxOszto) [(i, valodiOsztokSzama i) | i <- [a .. b]]
  where
    maxOszto = maximum osztokLs
    osztokLs = [valodiOsztokSzama i | i <- [a .. b]]

ketSzamKozottLegtobbValodiOszto = do
  putStr "Elso szam: "
  x1 <- readLn :: IO Int
  putStr "Masodik szam: "
  x2 <- readLn :: IO Int
  let lvosz = legtobbValodiOsztoSzam x1 x2
      losz = snd $ head lvosz
  putStr ("A legtobb oszto: " ++ show losz ++ ", az ezzel renelkezo szamok: ")
  putStrLn $ intercalate "," $ map (show . fst) lvosz

-- II. Írjunk egy-egy Haskell függvényt, amely beolvassa a billentyűzetről az n természetes számot és kiírja a képernyőre

-- - n-ig a Fibonacci számok listáját ($n > 50$), úgy hogy a számok közé szóközt ír,
fibonacci :: (Num a) => Int -> [a]
fibonacci n =
  let fib a b = a : fib b (a + b)
   in take n $ fib 0 1

fiboN = do
  n <- readLn :: IO Int
  let fiboLs = fibonacci n
  --   putStrLn $ unwords $ map show fiboLs
  mapM_ (\szam -> putStr $ show szam ++ " ") fiboLs

-- - n-ig a prímszámok listáját, úgy hogy a számok közé szóközt ír,
primSzamokN = do
  n <- readLn :: IO Int
  let primek = primszamok 1 n
  putStrLn $ unwords $ map show primek

-- - az n kettes számrendszerbeli alakját, úgy hogy minden negyedik bit után egy szóközt ír,
toBinary :: (Integral a) => a -> [a]
toBinary 0 = []
toBinary n = toBinary (div n 2) ++ [mod n 2]

toBinary2 :: Int -> String
toBinary2 0 = "0"
toBinary2 n = reverse (helper n)
  where
    helper 0 = ""
    helper x = (if mod x 2 == 1 then '1' else '0') : helper (div x 2)

chunksOf' :: Int -> [a] -> [[a]]
chunksOf' _ [] = []
chunksOf' n xs = take n xs : chunksOf' n (drop n xs)

formatBinary n = concat $ map (\x -> if length x == 4 then x ++ " " else x) $ chunksOf' 4 $ concat (map show . toBinary $ n)

formatBinary2 n = concatMap (\x -> if length x == 4 then x ++ " " else x) (chunksOf' 4 (showIntAtBase 2 intToDigit n ""))

formatBinary3 :: Int -> String
formatBinary3 n = intercalate " " (chunksOf' 4 (toBinary2 n))

printToBinary = do
  n <- readLn :: IO Int
  putStrLn (formatBinary n)

toBinary3 :: (Integral p) => p -> String
toBinary3 n = spacedBinary
  where
    binary = showIntAtBase 2 intToDigit n "" -- showIntAtBase - import Numeric, intToDigit - import Data.Char
    spacedBinary = unwords [take 4 $ drop i binary | i <- [0, 4 .. length binary - 1]]

toBinary2Szokoz :: Int -> String
toBinary2Szokoz 0 = "0"
toBinary2Szokoz n = reverse $ insertSpaces $ binaryHelper n
  where
    binaryHelper 0 = []
    binaryHelper x = (mod x 2) : binaryHelper (div x 2)
    insertSpaces [] = []
    insertSpaces xs =
      let (y, ys) = splitAt 4 xs
       in (concatMap show y) ++ " " ++ insertSpaces ys

-- - az n 16-os számrendszerbeli alakját, úgy hogy minden két szimbólum után egy szóközt ír, illetve az a, b, c, d, e, f szimbólumokat használja a 10-nél nagyobb számjegyek kódolására,
hexaN n = unwords $ chunksOf' 2 $ hexaSzj n
  where
    hexaSzj n1
      | n1 < 16 = [intToDigit n1]
      | otherwise = hexaSzj (div n1 16) ++ [intToDigit (mod n1 16)]

mainHexa = do
  putStr "n="
  n <- readLn :: IO Int
  let hN = hexaN n
  putStrLn hN

-- - az n értékének megfelelően a következő sorokat:
--   ```
--   (0, 0)
--   (0, 1) (1, 0)
--   (0, 2) (1, 1) (2, 0)
--   ...
--   (0, n) (1, n-1), ..., (n, 0)
--   ```
nSor n = [(i, n - i) | i <- [0 .. n]]

nSorok n = mapM_ (\sor -> putStrLn . unwords . map show $ sor) tabla
  where
    tabla = map nSor [0 .. n]

-- - az n értékének megfelelően az összes természetes szám kettes számrendszerbeli alakját,
--   például: $$n = 6:\ 0,\ 1,\ 10,\ 11,\ 100,\ 101,\ 110$$.
binN n = mapM_ (\x -> putStr $ x ++ ", ") [intercalate "" $ map show $ bin i | i <- [0 .. n]]
  where
    bin x
      | x < 2 = [x]
      | otherwise = bin (div x 2) ++ [mod x 2]

mainBin = do
  putStrLn "n="
  n <- readLn :: IO Int
  putStr (show n ++ ": ")
  binN n

-- III. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről olvas be egész számokat egy listába, majd kiírja a képernyőre, hogy
mainLs = do
  putStrLn "Adj meg szamokat szokozzel elvalasztva"
  ls <- getLine
  putStrLn "Adj meg egy szamot"
  n <- readLn :: IO Int
  let lsSzam = map (\n -> read n :: Int) $ words ls
      avg = atlag lsSzam
  print lsSzam
  putStrLn (show n ++ " elofordulasi szama " ++ show (elofN n lsSzam))
  putStrLn (show avg ++ " erteknel kisebb szamok: ")
  print $ atlagKisebb lsSzam
  mapM_ (\(i, j) -> putStrLn $ show i ++ " elof szama " ++ show j) $ elof lsSzam

-- - hányszor szerepel egy adott egész szám a listában,
elofN n ls = foldl (\res x -> if x == n then res + 1 else res) 0 ls

-- - melyek azok az egész számok, amelyek kisebbek a listában megadott számok átlagértékénél,
atlagKisebb ls = filter (\x -> fromIntegral x < atlag ls) ls

atlag ls = fromIntegral (sum ls) / fromIntegral (length ls)

-- - minden listabeli elem hányszor szerepel a listában, azaz készítsünk elem előfordulási statisztikát.
elof ls = map (\i -> (head i, length i)) . group . sort $ ls

-- IV. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről végjelig olvas be karakterláncokat, és
olvasVegjel = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- olvasVegjel
      return (line : rest)

mainVegjel = do
  ls <- olvasVegjel
  print ls
  putStrLn ("legnagyobb string: " ++ legnagyobbString ls)
  putStrLn ("legnagyobb string indexek: ")
  print $ legnagyobbIndex ls
  putStrLn "rendezett sorrend: "
  print . sort $ ls

-- - meghatározza a legnagyobbat,
legnagyobbString ls = maximumBy (comparing length) ls

-- - meghatározza a legnagyobb elemek indexét,
legnagyobbIndex ls = [j | (i, j) <- zip ls [0 ..], length i == maxHossz]
  where
    maxHossz = length $ legnagyobbString ls

-- - az adatok rendezett sorrendjét.