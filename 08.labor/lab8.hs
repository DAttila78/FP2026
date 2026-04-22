import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (isDigit)

fel1 :: [(String, Int)] -> Int -> IO ()
fel1 ls n = do
    let szurt = filter (\(_, nepesseg) -> nepesseg > n) ls
        rendezett = sortBy (comparing fst) szurt
        nevek = map fst rendezett

    if null nevek
        then putStrLn $ "Nincs " ++ show n ++ " erteknel nagyobb nepesseg ertekkel rendelkezo varos."
        else do
            putStrLn $ "A(z) " ++ show n ++ " nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:"
            mapM_ (\nev -> putStrLn $ "- " ++ nev) nevek

fel2 :: [Int] -> IO ()
fel2 ls = do
    let szurt = filter (\x -> '0' `notElem` show x) ls
        nevek = map show szurt

    if null nevek
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
        else putStrLn $ "A 0 szamjegyet nem tartalmazo szamok a kovetkezok: " ++ unwords nevek

fel3 :: [String] -> IO ()
fel3 ls = do
    let szurt = filter (\x -> not (any isDigit x)) ls
        rendezett = sortBy (comparing id) szurt

    if null rendezett
        then putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
        else do
            putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
            mapM_ putStrLn rendezett

main :: IO ()
main = do
    let ls1 = [ ("sepsiszentgyorgy", 54000)
            , ("kolozsvar", 330000)
            , ("marosvasarhely", 130000)
            , ("temesvar", 310000)
            , ("arad", 160000)
            , ("gyergyoszentmiklos", 18000)
            , ("nagyvarad", 196000)
            ]
    fel1 ls1 150000
    let ls2 = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
    fel2 ls2
    let ls3 = ["2023tuple", "function", "float", "higher-order", "variable10",
            "may13be", "0recursion", "monad", "class"]
    fel3 ls3