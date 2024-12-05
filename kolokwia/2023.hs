--zad 1
predicateZad1 :: String -> Bool
predicateZad1 x =
    3 * sum (map (\a -> fromEnum (a == 'a')) x) == length x


-- generatorZad1 :: Int -> [String] -> [String]
-- generatorZad1 0 x = x
-- generatorZad1 i [] = generatorZad1 (i-1) ["a", "b", "c"]
-- generatorZad1 i x = generatorZad1 (i-1) (map ('a' :) x ++ map ('b' :) x ++ map ('c' : ) x)

-- zad1 :: Int -> [String]
-- zad1 i = filter predicateZad1 (generatorZad1 (3*i) [])

zad1 :: Int -> [String]
zad1 i = filter predicateZad1 (iterate (\x -> map ('a' :) x ++ map ('b' :) x ++ map ('c' : ) x) ["a", "b", "c"] !! (3*i-1))

--zad2
collatzLoop :: Integer -> Integer
collatzLoop 1  = 0
collatzLoop x  = 1 + collatzLoop (if even x then div x 2 else 3*x+1)

-- collatzGenerator :: Integer -> Integer -> [Integer] -> [Integer]
-- collatzGenerator x i vals
--                     | i > x = vals
--                     | otherwise = collatzGenerator x (i+1) (vals ++ [collatzLoop i 0])

-- collatz :: Integer -> [Integer]
-- collatz x = collatzGenerator x 1 []

collatz :: Integer -> [Integer]
collatz x = map collatzLoop [1..x]

--zad3

-- type Wnr a = [(a, a, [a])]

-- wp :: Num a => Wnr a
-- wp = [(0, 0, [])]

-- dl :: (Num a, Ord a) => Wnr a -> a -> Wnr a
-- dl wnr x
--     | null vals = (x, x, [x]) : otherSegments
--     | x > max = (x, min, x : vals) : otherSegments
--     | x < min = (max, x, x : vals) : otherSegments
--     | otherwise = (max, min, x : vals) : otherSegments
--     where
--         (max, min, vals) = head wnr
--         otherSegments = tail wnr
--         -- max = fst firstSegment
--         -- min = head (tail firstSegment)
--         -- vals = x : tail (tail firstSegment)

-- ts :: Num a => Wnr a -> Wnr a
-- ts wnr
--     | null vals = wnr
--     | otherwise  = (0, 0, []) : wnr
--     where (min, max, vals) = head wnr

-- wmax :: Wnr a -> [a]
-- wmax wnr
--     | null vals = wmax (tail wnr)
--     | otherwise = map (\(max, min, vals) -> max) wnr
--     where (min, max, vals) = head wnr


-- wmin :: Wnr a -> [a]
-- wmin wnr
--     | null vals = wmin (tail wnr)
--     | otherwise = map (\(max, min, vals) -> min) wnr
--     where (min, max, vals) = head wnr


type Wnr a = [[a]]

wp :: Wnr a
wp = [[]]

dl :: (Num a, Ord a) => Wnr a -> a -> Wnr a
dl wnr x
    | null firstSegment = [x, x, x] : otherSegments
    | x > max = (x : min : vals) : otherSegments
    | x < min = (max : x : vals) : otherSegments
    | otherwise = (max : min : vals) : otherSegments
    where
        firstSegment = head wnr
        otherSegments = tail wnr
        max = head firstSegment
        min = head (tail firstSegment)
        vals = x : tail (tail firstSegment)

ts :: Wnr a -> Wnr a
ts wnr
    | null (head wnr) = wnr
    | otherwise  = [] : wnr

wmax :: Wnr a -> [a]
wmax wnr
    | null (head wnr) = wmax (tail wnr)
    | otherwise = map head wnr


wmin :: Wnr a -> [a]
wmin wnr
    | null (head wnr) = wmin (tail wnr)
    | otherwise = map (head . tail) wnr

test = dl (dl (dl wp 5) 4) 6
test2 = ts (dl (dl (dl (ts (ts (dl test 7))) 3) 2) 4)
