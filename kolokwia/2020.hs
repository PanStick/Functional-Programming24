--zad 1
type Point = (Double, Double)

takeSmallest :: [(Point, Point, Double)] -> (Point, Point, Double) -> (Point, Point, Double)
takeSmallest [] x = x
takeSmallest xs (p01, p02, min) =
                    let (p11, p12, val) = head xs in
                    if val < min
                    then takeSmallest (tail xs) (p11, p12, val)
                    else takeSmallest (tail xs) (p01, p02, min)

calcDist :: Point -> Point -> Double
calcDist (x1, y1) (x2, y2) = sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))

-- minDist :: [Point] -> (Point, Point, Double)
-- minDist x = let px = foldMap (\a -> map (\b -> (a, b, calcDist a b)) x) x in
--             takeSmallest (tail px) (head px)

generator :: [Point] -> [(Point, Point, Double)] -> [(Point, Point, Double)]
generator [] px = px
generator (x:xs) px = let pxn = px ++ map (\a -> (x, a, calcDist x a)) xs in
                        generator xs pxn

minDist :: [Point] -> (Point, Point, Double)
minDist x = let px = generator x [] in
            takeSmallest (tail px) (head px)

--zad 2
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Eq

-- findPath :: Eq a => a -> Tree a -> [a]
-- findPath x Empty = []
-- findPath x (Node val tr1 tr2) = if val == x
--                                 then [x]
--                                 else
--                                     let l1 = findPath x tr1 in
--                                     if null l1
--                                     then let l2 = findPath x tr2 in
--                                         if null l2
--                                         then []
--                                         else val : l2
--                                     else val : l1

findPath :: Eq a => a -> Tree a -> [a]
findPath x Empty = []
findPath x (Node val tr1 tr2)
    | val == x = [x]
    | (not . null) l1 = val : l1
    | (not . null) l2 = val : l2
    | otherwise = []
    where 
        l1 = findPath x tr1
        l2 = findPath x tr2

t=Node 10 (Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty)) (Node 20 Empty Empty)

--zad 3
findInArr :: Integer -> [Integer] -> Bool
findInArr val = foldr (\ x -> (||) (val == x)) False

closedPerm :: [Integer] -> Bool
closedPerm x = foldr ((&&) . (`findInArr` x)) True [1..(fromIntegral (length x))]

cpLoop :: [Integer] -> [Integer] -> [Integer] -> Integer -> [Integer]
cpLoop left curr cps i
    | null left = cps
    | closedPerm newCurr = cpLoop newLeft newCurr (i : cps) (i+1)
    | otherwise = cpLoop newLeft newCurr cps (i+1) 
    where   newLeft = tail left
            newCurr = head left : curr

cp :: [Integer] -> [Integer]
cp x = reverse (cpLoop x [] [] 1)