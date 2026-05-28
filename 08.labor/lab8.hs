module Lab8 where

import           Data.Function (on)
import           Data.List     (find, groupBy, intercalate, sortOn)
import           Data.Ord
import           System.IO

data Fesztivalok = Fesztivalok
  { fEgyuttes  :: String
  , fFesztival :: String
  , fAr        :: Int
  , fKod       :: Int
  } deriving (Show)

fesztivalok :: [Fesztivalok]
fesztivalok =
  [ Fesztivalok "Coldplay" "Glastonbury" 5000 101,
    Fesztivalok "Radiohead" "Coachella" 4500 102,
    Fesztivalok "Arctic Monkeys" "Glastonbury" 6000 103,
    Fesztivalok "Foo Fighters" "Lollapalooza" 4000 104,
    Fesztivalok "The Killers" "Coachella" 5500 105,
    Fesztivalok "Muse" "Lollapalooza" 3000 106,
    Fesztivalok "Imagine Dragons" "Glastonbury" 7000 107,
    Fesztivalok "Tame Impala" "Coachella" 3500 108,
    Fesztivalok "Red Hot Chili Peppers" "Lollapalooza" 8000 109,
    Fesztivalok "The Strokes" "Glastonbury" 2500 110
  ]

fesztEgyuttesek :: [Fesztivalok] -> String -> Maybe (String, [String])
fesztEgyuttesek ls fesztival =
  let rendezett = sortOn fFesztival ls
      csoportositott = groupBy ((==) `on` fFesztival) rendezett
      fE = map (\cs -> (fFesztival (head cs), map fEgyuttes cs)) csoportositott
   in find (\cs -> fst cs == fesztival) fE

olcsobbJegyek :: Int -> Maybe [String]
olcsobbJegyek ar = if null egyuttesek then Nothing else Just egyuttesek
  where
    egyuttesek = map fEgyuttes $ filter (\f -> fAr f < ar) fesztivalok

                    -- maybe -- > a nincs olyan esetek kezelesere valo 

olcsobbJegyekDb :: Int -> Maybe Int
olcsobbJegyekDb ar = if egyuttesekDb == 0 then Nothing else Just egyuttesekDb
  where
    egyuttesekDb = length $ filter (\f -> fAr f < ar) fesztivalok

-- a vegetol fogva rendez (altalanos beszuro rendezes)

insertSort :: (a -> a -> Bool) -> [a] -> [a]
insertSort _ [] = []
insertSort cmp (x:xs) = insert cmp x (insertSort cmp xs)
  where
    insert _ y [] = [y]
    insert cmp' y (z:zs) = if cmp' y z then y:z:zs else z : insert cmp' y zs

rendezEgyuttesekNev :: [Fesztivalok]
rendezEgyuttesekNev = insertSort (\f1 f2 -> fEgyuttes f1 < fEgyuttes f2) fesztivalok

-- klasszikus quicksort , elso elem a kozeppont s onnan szamolunk balra vagy jobbra 
quickS :: (a -> a -> Bool) -> [a] -> [a]
quickS _ [] = []
quickS cmp (k : ve) = quickS cmp kLs ++ [k] ++ quickS cmp nLs
  where
    kLs = [x | x <- ve, cmp x k]
    nLs = [x | x <- ve, not (cmp x k)]


rendezJegyarCsokk :: [Fesztivalok]
rendezJegyarCsokk = quickS (\f1 f2 -> fAr f1 > fAr f2) fesztivalok


-- oszd meg es uralkodj algoritmus , ket list osszefesulese
merge :: (t -> t -> Bool) -> [t] -> [t] -> [t]
merge _ [] ls = ls
merge _ ls [] = ls
merge cmp ls1@(k1 : ve1) ls2@(k2 : ve2)
  | cmp k1 k2 = k1 : merge cmp ve1 ls2
  | otherwise = k2 : merge cmp ls1 ve2

mergeS :: (a -> a -> Bool) -> [a] -> [a]
mergeS _ [] = []
mergeS _ [k] = [k]
mergeS cmp ls = merge cmp bLista jLista
  where
    db = div (length ls) 2
    bLista = mergeS cmp (take db ls)
    jLista = mergeS cmp (drop db ls)

rendezKod :: [Fesztivalok]
rendezKod = mergeS (\f1 f2 -> fKod f1 < fKod f2) fesztivalok

jegyekAtlagFeszt :: String -> Float
jegyekAtlagFeszt fesztival =
  let arak = map fAr $ filter (\f -> fFesztival f == fesztival) fesztivalok
      atlagAr = if null arak then 0 else fromIntegral (sum arak) / fromIntegral (length arak)
  in atlagAr

insertSort2 :: Ord a => [a] -> [a]
insertSort2 [] = []
insertSort2 (x:xs) = insert x (insertSort2 xs)
  where
    insert y [] = [y]
    insert y (z:zs) = if y <= z then y:z:zs else z : insert y zs

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] ls = ls
merge2 ls [] = ls
merge2 ls1@(k1:ve1) ls2@(k2:ve2)
  | k1 < k2 = k1 : merge2 ve1 ls2
  | otherwise = k2 : merge2 ls1 ve2

mergeS2 :: Ord a => [a] -> [a]
mergeS2 [] = []
mergeS2 [k] = [k]
mergeS2 ls = merge2 bLista jLista
  where
    db = div (length ls) 2
    bLista = mergeS2 (take db ls)
    jLista = mergeS2 (drop db ls)

data Varos = Varos {
  vNev      :: String,
  vNepSzam  :: Int,
  vTerMeret :: Int
} deriving (Show)


split :: Char -> String -> [String]
split _ [] = [[]]
split delim (c : cs)
  | c == delim = [] : rest
  | otherwise = (c : head rest) : tail rest
  where rest = split delim cs

toVaros :: String -> Varos
toVaros line = Varos nev (read nepesseg) (read terMeret)
  where [nev, nepesseg, terMeret] = split ',' line

varosNepsuruseg :: [Varos] -> [(String, Float)]
varosNepsuruseg varosok = [(vNev v, fromIntegral (vNepSzam v) / fromIntegral (vTerMeret v)) | v <- varosok]

nepsurusegABFg :: Float -> Float -> [Varos] -> [(String, Float)]
nepsurusegABFg a b varosok
  | a < b     = filter (\v -> snd v > a && snd v < b) (varosNepsuruseg varosok)
  | otherwise = filter (\v -> snd v > b && snd v < a) (varosNepsuruseg varosok)

rendezNepsuruseg :: [Varos] -> [(String, Int, Int, Int)]
rendezNepsuruseg vLs = sortOn (\(_,x,_,_) -> x)
  [(vNev v, div (vNepSzam v) (vTerMeret v), vNepSzam v, vTerMeret v) | v <- vLs]

data BST = Empty | Node Varos BST BST deriving (Show)

insertBST :: Varos -> BST -> BST
insertBST v Empty = Node v Empty Empty
insertBST v (Node x l r)
  | vNepSzam v < vNepSzam x = Node x (insertBST v l) r
  | otherwise = Node x l (insertBST v r)

buildBST :: [Varos] -> BST
buildBST = foldr insertBST Empty

inOrder :: BST -> [Varos]
inOrder Empty = []
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r

minVaros :: BST -> Varos
minVaros (Node v Empty _) = v
minVaros (Node _ l _) = minVaros l

maxVaros :: BST -> Varos
maxVaros (Node v _ Empty) = v
maxVaros (Node _ _ r) = maxVaros r

writeToFile :: FilePath -> [Varos] -> IO ()
writeToFile path vs = writeFile path (unlines (map show vs))

type Name = String
type KeyLen = [Int]
type BlockLen = Int
type Protocol = String

data Crypto =
  StreamCipher Name KeyLen [Protocol]
  | BlockCipher Name KeyLen BlockLen [Protocol]
  | BlockCipherMode Name
  deriving (Show, Read, Eq)

lsCrypto = [
  BlockCipher "AES" [128, 192, 256] 128 ["TLS", "PGP", "Kerberos"],
  BlockCipherMode "ECB",
  BlockCipherMode "CBC",
  BlockCipher "Twofish" [128, 192, 256] 128 ["PGP", "Kerberos"],
  StreamCipher "ChaCha20" [128, 256] ["TLS", "S/MIME", "SSH"],
  BlockCipher "3DES" [168] 64 ["TLS", "PGP", "Kerberos"],
  BlockCipherMode "CTR",
  BlockCipherMode "GCM",
  StreamCipher "RC4" [40..2048] ["Kerberos"]
 ]

isBCM (BlockCipherMode _) = True
isBCM _ = False
bcmDb = length $ filter isBCM lsCrypto

getName :: Crypto -> Maybe String
getName (BlockCipherMode n) = Just n
getName _ = Nothing
bcmLs = filter isBCM lsCrypto

isBC (BlockCipher {}) = True
isBC _ = False
getBC (BlockCipher name keyLen blockLen protocols) = (name, keyLen, blockLen, protocols)
getProtocolCount (BlockCipher _ _ _ protocols) = length protocols
getProtocolCount _ = 0
bcLs = filter isBC lsCrypto
maxSzam = maximum $ map getProtocolCount bcLs
maxBC = map getBC $ filter (\c -> getProtocolCount c == maxSzam) bcLs

isSC (StreamCipher {}) = True
isSC _ = False
getSC (StreamCipher name keyLen protocols) = (name, keyLen, protocols)
haromelem1 (x,_,_) = x
scLs = sortOn haromelem1 $ map getSC $ filter isSC lsCrypto
scLsToFile = unlines $ map (\(n,k,p) -> n ++ " [" ++ intercalate "," (map show k) ++ "], [" ++ intercalate "," p ++ "]") scLs

mainI :: IO ()
mainI = do
  case fesztEgyuttesek fesztivalok "Glastonbury" of
    Nothing -> putStrLn "Nincs ilyen fesztivál"
    Just (_, egyuttesek) -> do
      putStrLn "Glastonbury együttesei:"
      mapM_ putStrLn egyuttesek

  let ar = 4000
  case olcsobbJegyek ar of
    Nothing -> putStrLn $ show ar ++ " árnál nincs olcsóbb jegy"
    Just egyuttesek -> putStrLn $ show ar ++ " árnál olcsóbb együttesek: " ++ intercalate ", " egyuttesek

  case olcsobbJegyekDb ar of
    Nothing -> return ()
    Just db -> putStrLn $ show ar ++ " árnál olcsóbb együttesek száma: " ++ show db

  putStrLn $ intercalate ", " $ map fEgyuttes rendezEgyuttesekNev

  mapM_ (\f -> putStrLn $ fEgyuttes f ++ " " ++ show (fAr f)) rendezJegyarCsokk

  mapM_ (\f -> putStrLn $ fEgyuttes f ++ " " ++ show (fKod f)) rendezKod

  putStrLn $ "Glastonbury jegyeinek átlaga: " ++ show (jegyekAtlagFeszt "Glastonbury")

mainII :: IO ()
mainII = do
  inf <- openFile "08.labor/varosok.txt" ReadMode
  tartalom <- hGetContents inf
  let varosok = map toVaros (lines tartalom)
  hClose inf

  putStr "a="; a <- readLn :: IO Float
  putStr "b="; b <- readLn :: IO Float
  let nepsurusegAB = nepsurusegABFg a b varosok
  mapM_ (\(nev, sur) -> putStrLn $ nev ++ " " ++ show sur) nepsurusegAB
  putStrLn $ "Városok száma a megadott intervallumban: " ++ show (length nepsurusegAB)

  mapM_ (\(nev, ns, n, t) -> putStrLn $ nev ++ " " ++ show ns ++ " = " ++ show n ++ " / " ++ show t)
        (rendezNepsuruseg varosok)

  let bstVarosok = buildBST varosok
  writeToFile "08.labor/rendezett_varosok.txt" (inOrder bstVarosok)
  putStrLn $ "Legkisebb népességű város: " ++ show (minVaros bstVarosok)
  putStrLn $ "Legnagyobb népességű város: " ++ show (maxVaros bstVarosok)

mainIII :: IO ()
mainIII = do
  putStrLn $ "A lista " ++ show bcmDb ++ " darab BlockCipherMode tipusu adatot tarol:"
  mapM_ (\n -> case getName n of Just name -> putStrLn name; _ -> return ()) bcmLs

  mapM_ (\(n,k,b,p) -> putStrLn $ n ++ " [" ++ intercalate ", " (map show k) ++ "] " ++ show b ++ " [" ++ intercalate ", " p ++ "]") maxBC

  writeFile "08.labor/sc_adatok.txt" scLsToFile

main :: IO ()
main = mainI >> mainII >> mainIII