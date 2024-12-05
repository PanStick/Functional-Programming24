--zad 1
czyPodzielny4 :: [Int] -> Bool
czyPodzielny4 x = mod (sum x) 4 == 0

-- sprawdzajPodciagi :: [[Int]] -> Int
-- sprawdzajPodciagi x = if any czyPodzielny4 x
--                      then length (head x)
--                      else sprawdzajPodciagi (map init x ++ map tail x)

-- cztery :: [Int] -> Int
-- cztery x = sprawdzajPodciagi [x]

cztery :: [Int] -> Int
cztery x = length (head (head (filter (any czyPodzielny4) (iterate (\a -> map init a ++ map tail a) [x]))))


--zad 2
trim :: String -> String
trim x
    | head x == ' ' = trim (tail x)
    | last x == ' ' = trim (init x)
    | otherwise = x

convertToGrade :: Double -> Double -> String
convertToGrade max p
    | p > max || p < 0 = "Nieprawidłowe dane"
    | p > max * 0.9 = "5.0"
    | p > max * 0.8 = "4.5"
    | p > max * 0.7 = "4.0"
    | p > max * 0.6 = "3.5"
    | p > max * 0.5 = "3.0"
    | otherwise = "2.0"


wyniki :: Double -> [(String, Double)] -> [(String, String)]
wyniki p = map (\(a, b) -> (trim a, convertToGrade p b))

--zad 3

data Bsk a = Empty | El [a] a Integer
 
--deriving instance (Show a) => Show (Bsk a)
 
de :: (Eq a) => Bsk a -> a -> Bsk a
de Empty x = El [x] x 1
de (El els fs count) x = El (x : els) fs (if fs == x then count + 1 else count)
 
oe :: Bsk a -> a
oe (El (el : els) fs count) = el
 
ue :: (Eq a) => Bsk a -> Bsk a
ue (El (el : els) fs count) = El els fs (if el == fs then count - 1 else count)
 
le :: (Eq a) => Bsk a -> Integer
le (El _ _ count) = count
 
bsk2l :: Bsk a -> [a]
bsk2l (El els _ _) = els


-- type Bsk a = (Integer, a, [a])

-- de :: Eq a => Bsk a -> a -> Bsk a
-- de (c, f, vals) x
--     | null vals = (0, x, [x])
--     | x == f = (c+1, f, x:vals)
--     | otherwise = (c, f, x:vals)

-- oe :: Bsk a -> a
-- oe (c, f, vals) = head vals

-- ue :: Eq a => Bsk a -> Bsk a
-- ue (c, f, vals)
--     | head vals == f = (c-1, f, tail vals)
--     | otherwise = (c, f, tail vals)

-- le :: Eq a => Bsk a -> Integer
-- le (c, f, vals) = c

-- bsk21 :: Bsk a -> [a]
-- bsk21 (c, f, vals) = vals