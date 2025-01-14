-- import Data.Char (toUpper)


-- f :: String -> String
-- f [] = []
-- f (x:xs) = if toUpper x == x then x : f xs else f xs

-- wielkieLitery :: [String] -> [String]
-- wielkieLitery = map f

-- wielkieLitery :: [String] -> [String]
-- wielkieLitery = map (filter (\x -> x == toUpper x))

wielkieLitery :: [String] -> [String]
wielkieLitery = map (filter (\x -> x `elem` ['A'..'Z']))

--zad 1
maleLitery :: [String] -> [String]
maleLitery = map (filter (\x -> x `elem` ['a'..'z']))

--zad 2
dlugoscPalindromow :: [String] -> Int
dlugoscPalindromow x = sum (map length (filter (\n -> n == reverse n) x))
    --[n | n <- x, n == reverse n]

--zad 3
fib :: (Integer, Integer) -> [(Integer, Integer)]
fib (x, y) = iterate (\(a,b) -> (b, a+b)) (x, y)

fibList :: [Integer]
fibList = map fst (fib (0, 1))
--x = map fst fib (0, 1)

dlugosc :: [a] -> Int
dlugosc x = sum (map (const 1) x)

-- generatorSlow :: Char -> Char -> Integer -> [String] -> [String]
-- generatorSlow a b x xs = if x > 0
--                         then generatorSlow a b (x-1) (map (++[a]) xs ++ map (++[b]) xs)
--                         else xs

-- slowaDlugosci :: Char -> Char -> Integer -> [String]
-- slowaDlugosci a b 0 = []
-- slowaDlugosci a b x = generatorSlow a b (x-1) [[a], [b]]

--zad 5
slowaDlugosci :: Char -> Char -> Integer -> [String]
slowaDlugosci a b 0 = [""]
slowaDlugosci a b x = map (a :) prev ++ map (b :) prev
                        where prev = slowaDlugosci a b (x-1)

-- slowaDlugosci :: Char -> Char -> Int -> [String]
-- slowaDlugosci a b 0 = []
-- slowaDlugosci a b x = filter (\n -> length head n == x) (iterate (\n -> map(++[a]) n ++ map(++[b]) n) [[a], [b]])

--zad 6
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort x = let n = head x in
        quickSort (filter (< n) x) ++ filter (==n) x ++ quickSort (filter (> n) x)

--zad7
funkcjaMap :: (a -> b) -> [a] -> [b]
funkcjaMap f xs = zipWith (\x y -> (f x)) xs xs
 
funkcjaZip :: [a] -> [b] -> [(a,b)]
funkcjaZip xs ys = zipWith (\x y -> (x,y)) xs ys

--zad 8
eratosthenes :: Integer -> [Integer]
eratosthenes n = eratosthenes' [2 .. n]
  where
    eratosthenes' [] = []
    eratosthenes' (x : xs) = x : eratosthenes' (filter (\y -> y `mod` x /= 0) xs)