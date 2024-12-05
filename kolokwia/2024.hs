--zad 1

mozliwyDoWyplacenia :: Integer -> Integer -> Integer -> Integer -> Bool
mozliwyDoWyplacenia a b val target
    | val > target = False
    | val == target = True
    | otherwise = mozliwyDoWyplacenia a b (val+a) target || mozliwyDoWyplacenia a b (val+b) target

wypłata :: Integer -> Integer -> Integer -> [Integer]
wypłata a b n = filter (mozliwyDoWyplacenia a b 0) [1..n]

--zad 2

generatorSłow :: Integer -> Integer -> [String] -> [String]
generatorSłow l i s
    |  i == l = s
    | otherwise = s ++ generatorSłow l (i+1) (('b' : head s) : map ('a' :) s)

predicate :: Integer -> Integer -> String -> Bool
predicate i j s = aCount <= i && bCount <= j && aCount /= bCount
    where
        aCount = fromIntegral (length (filter (=='a') s))
        bCount = fromIntegral (length s) - aCount

słowa :: Integer -> Integer -> [String]
słowa i j =
    filter (predicate i j) (generatorSłow (i+j) 1 ["b", "a"] )


--zad 3

data Gkt a = Gkt (Gkt a -> Bool) Int [a]

gp :: (Gkt a -> Bool) -> Gkt a
gp f = Gkt f 0 []

de :: a -> Gkt a -> Gkt a
de val (Gkt f i vals) =
    if f (Gkt f i vals)
        then Gkt f (i+1) [val]
        else Gkt f i (val:vals)

oe :: Gkt a -> a
oe (Gkt f i vals) = head vals

ue :: Gkt a -> Gkt a
ue (Gkt f i []) = Gkt f i []
ue (Gkt f i vals) = Gkt f i (tail vals)

loo :: Gkt a -> Int
loo (Gkt f i vals) = i

g21 :: Gkt a -> [a]
g21 (Gkt f i vals) = vals

testFunc :: Gkt a -> Bool
testFunc (Gkt f i vals) = length vals == 2

test = de 4 (de 3 (gp testFunc))
test2 = loo (de 2 test)
