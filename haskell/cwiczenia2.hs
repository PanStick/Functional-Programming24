
jestPunktemStalym :: (Eq a) => a -> (a -> a) -> Bool
jestPunktemStalym x f = x == f x


f :: (Real a, Show a) => a -> a -> String
f x y = "iloczyn " ++ show x ++ " i " ++ show y
         ++ (if x*y <= x+y then " nie " else " ")
         ++ "jest wiekszy od ich sumy"


--[n | n <- [1..], mod n 6 == 1, mod n 7 == 4, mod n 8 == 3]

dzielniki :: Int -> [Int]
dzielniki x = [n | n <- [1..(x-1)], mod x n == 0]

{-
wyznaczCwiartke :: (Int, Int) -> [Int]
wyznaczCwiartke (x, y)
                | x > 0 && y > 0 = [1, 0, 0, 0]
                | x > 0 && y < 0 = [0, 0, 0, 1]
                | x < 0 && y > 0 = [0, 1, 0, 0]
                | x < 0 && y < 0 = [0, 0, 1, 0]
                | x == 0 && y == 0 = [1, 1, 1, 1]
                | x < 0 = [0, 1, 1, 0]
                | x > 0 = [1, 0, 0, 1]
                | y > 0 = [1, 1, 0, 0]
                | y < 0 = [0, 0, 1, 1]
-}

{-
g :: (Int, Int) -> Bool
g (x, y) = x >= 0 && y >= 0

policzCwiartke :: [(Int, Int)] -> ((Int, Int) -> Bool) -> Int
policzCwiartke xs f = sum [1 | x <- xs, f x]

ktoraCwiartka :: [(Int, Int)] -> Int
ktoraCwiartka xs = let pierwszaCwiartka = policzCwiartke xs ((fst >=0)&&(snd >=0)) in 0
-}

                    
{-
policzCwiartki :: [(Int, Int)] -> [Int]-- -> [Int]
policzCwiartki xs ys
                | xs null = ys
                | otherwise policzCwiartki 

ktoraCwiartka :: [(Int, Int)] -> Int
ktoraCwiartka = findIndex (max policzCwiartki [0, 0, 0, 0]) + 1
-}

podlisty :: [Integer] -> [[Integer]]
podlisty [] = [[]]
podlisty (x:xs) = [x : podlista | podlista <- podlisty xs] ++ podlisty xs

podlistyDlugosci :: Integer -> [Integer] -> [[Integer]]
podlistyDlugosci k xs = [x | x <- podlisty xs, length x == fromIntegral k]
--sum [1 | _ <- list]

przeksztalcListe :: (Int -> Int) -> [Int] -> [Int]
przeksztalcListe f (x:xs) = f x : przeksztalcListe f xs
przeksztalcListe _ _ = []

przeksztalcListe2 :: (Int -> Int) -> [Int] -> [Int]
przeksztalcListe2 f xs = [f x | x <- xs]
