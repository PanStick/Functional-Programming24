--zad 2

-- sprawdzaczoinator :: Int -> Int -> [[Int]] -> [Int] -> [Int]
-- sprawdzaczoinator x i l w = if i > x then w
--     else sprawdzaczoinator x (i+1) l (sum (map (fromEnum . elem i) l) : w)

-- wIluListach :: Int -> [[Int]] -> [Int]
-- wIluListach x l = reverse (sprawdzaczoinator x 1 l [])

wIluListach :: Int -> [[Int]] -> [Int]
wIluListach x l = map (\i -> length (filter (elem i) l)) [1..x]

-- wIluListach :: Int -> [[Int]] -> [Int]
-- wIluListach n lists = map countOccurrences [1..n]
--   where
--     countOccurrences x = length (filter (x `elem`) lists)

--zad 1
type DirectedGraph = ([Int], Int -> Int -> Bool)

-- travel :: DirectedGraph -> Int -> [Int] -> [Int]
-- travel (vs, f) 0 vst = vst
-- travel (vs, f) d vst = travel (vs, f) (d-1) (concatMap (\a -> filter (f a) vs) vst)

-- atDistance :: DirectedGraph -> Int -> Int -> [Int]
-- atDistance g d v = travel g d [v]

atDistance :: DirectedGraph -> Int -> Int -> [Int]
atDistance (vs, f) d v = iterate (concatMap (\a -> filter (f a) vs)) [v] !! d

--zad 3

type Wr a = [[a]]

dg :: Wr a -> [a] -> Wr a
dg wr x = x : wr

ug :: Wr a -> Wr a
ug [[]] = [[]]
ug (x:xs) = xs