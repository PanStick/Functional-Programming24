
func1 :: Double -> Double -> Double
func1 x y = (x^2 + y^2) / (2 * x * y)

func2 :: Double -> Double
func2 x = func1 x 1

dodajJeden :: Int -> Int
dodajJeden x = x + 1

sumaWartosci :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
sumaWartosci f g x y = f x + g y

func4 :: Int -> Int
func4 x = 2 * x

func5 :: Int -> Int
func5 x = 3 * x

func3 :: Int -> Int -> Int
func3 x y = sumaWartosci func4 func5 x y

ocena :: Double -> String
ocena x
    | x == 2.0 = "niezaliczone"
    | x == 5.0 = "brawo!"
    | otherwise = "wpisane masz" ++ show x

--zad 4
stirling :: Integer -> Integer -> Integer
stirling n k
         | n == 0 = 1
         | k == 0 = 0
         | k == n = 1
         | otherwise  = ((n-1) * stirling (n-1) k)
         + stirling (n-1) (k-1)

--zad 5
iloczynListy :: [Integer] -> Integer
iloczynListy xs
    | null xs = 1
    | otherwise = head xs * iloczynListy (tail xs)

--iloczynListy [] = 1
--iloczynListy (x:xs) = x * iloczynListy xs

--iloczynListy xs = foldr (*) 0 xs

--zad 6
merge :: [Int] -> [Int] -> [Int]
merge xs ys
    | null xs = ys
    | null ys = xs
    | head ys >= head xs = head xs : merge (tail xs) ys
    | head xs > head ys = head ys : merge xs (tail ys)

mergeSort :: [Int] -> [Int]
mergeSort xs
    | length xs == 1 = xs
    | otherwise = let n = div (length xs) 2
                    in merge (mergeSort (take n xs)) (mergeSort (drop n xs))
    
    --head xs > head ys = head ys : merge xs (tail ys)
    {-
    | otherwise = [head xs, head ys] 
    ++ merge (tail xs) (tail ys)
    -}


--zad 7
znajdzNastepnyDzielnik :: Int -> Int -> Int
znajdzNastepnyDzielnik x y
    | mod x y == 0 = y
    | otherwise = znajdzNastepnyDzielnik x (y+1)

dzielniki :: (Int, [Int]) -> [Int]
dzielniki (x, y) = let n = znajdzNastepnyDzielnik x (last y + 1)
                    in if x == n then y else dzielniki (x, y ++ [n])

czyDoskonala :: Int -> Bool
czyDoskonala x = x == sum (dzielniki (x, [1]))

--if znajdzNastepnyDzielnik (x, last y) as n == x then y else dzielniki(x, y ++ [n])


--dzielniki :: (Int, [Int]) -> [Int]
--dzielniki (x, xs) = xs ++ [n | n <- [2..(x-1)], mod x n == 0]


{-
czyDoskonala :: Int -> Bool
czyDoskonala x = x == sum [n | n <- [1..(x-1)], mod x n == 0]
-}