module Lab10 where

import           Data.List     (intercalate, maximumBy, sort, sortBy, sortOn)
import           Data.Ord      (comparing, Down(..))
import           System.IO

type Nev = String
type Hash = String

kulcsI :: String -> String
kulcsI jelszo = jelszo ++ "salt" ++ reverse jelszo

kulcsII :: String -> [String]
kulcsII jelszo = [take n (jelszo ++ "salt" ++ reverse jelszo ++ repeat 'x') | n <- [16,32,64]]

findPassword :: [(Nev, Hash)] -> String -> [(Nev, String)]
findPassword ls target =
  [(nev, kulcs) | (nev, h) <- ls, kulcs <- kulcsI target : kulcsII target, h == target]

toRoman :: Int -> String
toRoman n = concat $ zipWith replicate ns rs
  where
    ns = [n `div` v | v <- vs]
    rs = ["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"]
    vs = [1000,900,500,400,100,90,50,40,10,9,5,4,1]

fromRoman :: String -> Int
fromRoman s = sum $ map val $ groups s
  where
    val "M"=1000; val "CM"=900; val "D"=500; val "CD"=400
    val "C"=100; val "XC"=90; val "L"=50; val "XL"=40
    val "X"=10; val "IX"=9; val "V"=5; val "IV"=4; val "I"=1; val _=0
    groups [] = []
    groups (a:b:xs) | [a,b] `elem` ["CM","CD","XC","XL","IX","IV"] = [a,b]:groups xs
    groups (a:xs) = [a]:groups xs

romanOp :: (Int -> Int -> Int) -> String -> String -> String
romanOp op r1 r2 = toRoman $ fromRoman r1 `op` fromRoman r2

data Fesztivalok = Fesztivalok {
  fFesztival :: String,
  fKod       :: Int,
  fAr        :: Int,
  fEgyuttes  :: [String]
} deriving (Show)

legtobbEgyuttes :: [Fesztivalok] -> (String, Int)
legtobbEgyuttes fs = maximumBy (comparing snd) [(fFesztival f, length (fEgyuttes f)) | f <- fs]

egyuttesSzam :: [Fesztivalok] -> [(String, Int)]
egyuttesSzam fs = [(fFesztival f, length (fEgyuttes f)) | f <- fs]

rendezJegyAr :: [Fesztivalok] -> [Fesztivalok]
rendezJegyAr = sortBy (comparing fAr)

data BST a = Empty | Node a (BST a) (BST a) deriving (Show)

insertBST :: Ord b => (a -> b) -> a -> BST a -> BST a
insertBST _ v Empty = Node v Empty Empty
insertBST key v (Node x l r)
  | key v < key x = Node x (insertBST key v l) r
  | otherwise     = Node x l (insertBST key v r)

inOrder :: BST a -> [a]
inOrder Empty = []
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r

data Olimpia = Olimpia {
  oOrszag    :: String,
  oSportagak :: [(String, Int)]
} deriving (Show)

totalErmek :: Olimpia -> Int
totalErmek o = sum (map snd (oSportagak o))

legtobbErmes :: [Olimpia] -> (String, Int)
legtobbErmes os = maximumBy (comparing snd) [(oOrszag o, totalErmek o) | o <- os]

sportagak :: [Olimpia] -> [String]
sportagak = sort . concatMap (map fst . oSportagak)

sportagErmekDb :: String -> [Olimpia] -> Int
sportagErmekDb s os = sum [e | o <- os, (sp, e) <- oSportagak o, sp == s]

orszagRendezett :: String -> [Olimpia] -> [(String, Int)]
orszagRendezett orsz os = 
  sortBy (comparing (Down . snd)) [(sp, e) | o <- os, oOrszag o == orsz, (sp, e) <- oSportagak o]

data RealBST = EmptyR | NodeR Double RealBST RealBST deriving (Show)

insertReal :: Double -> RealBST -> RealBST
insertReal v EmptyR = NodeR v EmptyR EmptyR
insertReal v (NodeR x l r)
  | v < x     = NodeR x (insertReal v l) r
  | otherwise = NodeR x l (insertReal v r)

inOrderReal :: RealBST -> [Double]
inOrderReal EmptyR = []
inOrderReal (NodeR v l r) = inOrderReal l ++ [v] ++ inOrderReal r

sumReal :: RealBST -> Double
sumReal EmptyR = 0
sumReal (NodeR v l r) = v + sumReal l + sumReal r

mainI :: IO ()
mainI = do
  tartalom <- readFile "10.labor/jelszavakNevek.txt"
  let adatok = [(nev, hash) | [nev, hash] <- map words (lines tartalom)]
  print $ findPassword adatok "LLEKSAH"
  print $ findPassword adatok "masPSWD123"

mainII :: IO ()
mainII = do
  putStr "Első római szám: "; r1 <- getLine
  putStr "Második római szám: "; r2 <- getLine
  putStrLn $ "Összeg: " ++ romanOp (+) r1 r2
  putStrLn $ "Szorzat: " ++ romanOp (*) r1 r2
  putStrLn $ "Különbség: " ++ romanOp (-) r1 r2
  putStrLn $ "Hányados: " ++ romanOp div r1 r2
  let arabRoman = [(i, toRoman i) | i <- [1..3999]]
  writeFile "10.labor/arab_roman.txt" $ unlines [show a ++ " " ++ r | (a,r) <- arabRoman]

mainIII :: IO ()
mainIII = do
  let fesztivalok = [] -- adatfájl betöltése ha szükséges
  print $ legtobbEgyuttes fesztivalok
  print $ egyuttesSzam fesztivalok
  mapM_ print $ rendezJegyAr fesztivalok
  let bst = foldr (insertBST fFesztival) Empty fesztivalok
  print $ map fFesztival $ inOrder bst

mainIV :: IO ()
mainIV = do
  putStrLn "Olimpia adatok beolvasása szükséges"
  let olimpiak = [] -- adatfájl betöltése ha szükséges
  print $ legtobbErmes olimpiak
  print $ sportagak olimpiak
  putStr "Sportág: "; sport <- getLine
  print $ sportagErmekDb sport olimpiak
  putStr "Ország: "; orsz <- getLine
  print $ orszagRendezett orsz olimpiak
  let bst = foldr (insertBST oOrszag) Empty olimpiak
  mapM_ print $ inOrder bst

mainV :: IO ()
mainV = do
  let szamok = [3.14, 2.71, 1.41, 5.0, 0.5, 10.0]
  let fa = foldr insertReal EmptyR szamok
  print $ inOrderReal fa
  print $ sumReal fa

main :: IO ()
main = do
  mainI
  mainII
  mainIII
  mainIV
  mainV