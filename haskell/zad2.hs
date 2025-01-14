jestPunktemStalym :: (Eq a) => a -> (a -> a) -> Bool
jestPunktemStalym x f = x == f x

f :: (Num a, Ord a, Show a) => a -> a -> String
f x y = "iloczyn " ++ show x ++ " i " ++ show y ++
        (if (x*y) <= (x+y) then " nie " else " ")
         ++ "jest wiekszy od ich sumy"

-- [x | x <-[1..], mod x 6 == 5, mod x 7 == 3, mod x 8 == 5]

dzielniki :: Int -> [Int]
dzielniki x = [n | n <- [1..(x-1)], mod x n == 0]

podlisty :: [Integer] -> [[Integer]]
podlisty [] = [[]]
podlisty (x:xs) = [x : podlista | podlista <- podlisty xs] ++ podlisty xs

podlistyDlugosci :: Integer -> [Integer] -> [[Integer]]
podlistyDlugosci k = filter (\x -> length x == fromIntegral k).podlisty


--zad 7
przeksztalcListe :: (Int -> Int) -> [Int] -> [Int]
przeksztalcListe f xs = [f x | x <- xs]

kwadrat10 :: [Int]
kwadrat10 = przeksztalcListe (^2) [1..10]
