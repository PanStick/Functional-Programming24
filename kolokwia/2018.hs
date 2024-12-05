--zad 3
cykl :: [a] -> [[a]]
cykl [] = [[]]
cykl l = foldl (\acc x -> acc ++ [tail (last acc) ++ [head (last acc)]]) [l] (tail l)

-- --zad 1
-- a :: Int -> Int
-- a 0 = 1
-- a n = (n - 1) * (b (n-1)) - 3*(a(n-1))

-- b :: Int -> Int
-- b 0 = 1
-- b n = 3 * b(n-1) + (n-1)^2 * a(n-1) - (n-1)^2

-- takeWhileSum :: [Int] -> Int -> Int -> Int
-- takeWhileSum (x:xs) m n | (m - x) < 0 = n
--                         | otherwise = takeWhileSum xs (m - x) (n + 1)

-- seqIndex :: Int -> Int
-- seqIndex m = takeWhileSum (map a [1..]) m 1

--zad 1

seriesCalc :: Int -> Int -> Int -> Int -> Int -> Int
seriesCalc a b n sum target
    | sum >= target = n
    | otherwise =
        let an = (n-1)*b - 3*a in
        seriesCalc an (3*b+(n-1)*(n-1)*a -(n-1)*(n-1)) (n+1) (sum+an) target

seqIndex :: Int -> Int
seqIndex = seriesCalc 1 1 0 0

--zad 2
data Expr a = Value a
    | Add (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | P
    deriving (Eq, Show)

-- add :: (Num a, Eq a) => Expr a -> Expr a -> Expr a
-- add P _ = P
-- add _ P = P
-- add (Value x1) (Value x2) = Value (x1+x2)
-- add a b = add (calcExpression a) (calcExpression b)

-- mult :: (Num a, Eq a) => Expr a -> Expr a -> Expr a
-- mult P _ = P
-- mult _ P = P
-- mult (Value x1) (Value x2) = Value (x1*x2)
-- mult a b = mult (calcExpression a) (calcExpression b)

-- sub :: (Num a, Eq a) => Expr a -> Expr a -> Expr a
-- sub P _ = P
-- sub _ P = P
-- sub (Value x1) (Value x2) = Value(x1-x2)
-- sub a b = sub (calcExpression a) (calcExpression b)

apply :: (Num a, Eq a) => (a -> a -> a) -> Expr a -> Expr a -> Expr a
apply _ _ P = P
apply _ P _ = P
apply f (Value x1) (Value x2) = Value (f x1 x2)
apply f a b = apply f (calcExpression a) (calcExpression b)


calcExpression :: (Num a, Eq a) => Expr a -> Expr a
calcExpression (Value x) = Value x
calcExpression (Add a b) = apply (+) a b
calcExpression (Mul a b) = apply (*) a b
calcExpression (Sub a b) = apply (-) a b
calcExpression P = P

eq :: (Eq a, Num a) => Expr a -> Expr a -> Bool
eq a b 
    | res1 == P || res2 == P || res1 == res2 = True
    | otherwise = False
    where 
    res1 = calcExpression a
    res2 = calcExpression b