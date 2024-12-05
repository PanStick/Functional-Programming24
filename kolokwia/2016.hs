--zad 1

breakNum :: Integer ->  [Integer] -> [Integer]
breakNum x xs
    | x < 10 = x:xs
    | x >= 10 = breakNum (div x 10) (mod x 10:xs)

pownum :: Integer -> [Integer]
pownum x = filter (\a -> sum (map (^x) (breakNum a [])) == a) [1..]

--zad 2

ps :: [a] -> [[a]]
-- ps x = foldl (\a b -> a ++ [last a ++ [b]] ) [[head x]] (tail x)
ps x = foldl (\a b -> a ++ [last a ++ [b]] ) [[head x]] (tail x) ++ tail (foldr (\a b -> (a : head b) : b) [[last x]] (init x))

--zad 3
data Rd a = Rd a [Rd a]
    deriving Show

e1 :: Eq a => Rd a -> a -> Bool
e1 (Rd v rds) val = v == val || any (`e1` val) rds

subst :: Eq a => a -> a -> Rd a -> Rd a
subst x y (Rd v rds) =
    let rs = map (subst x y) rds in
    if v == x
        then Rd y rs
        else Rd v rs

rd2list :: Rd a -> [a]
rd2list (Rd v rds) = v : concatMap rd2list rds

test = Rd 3 [Rd 2 [Rd 3 []], Rd 4 []]