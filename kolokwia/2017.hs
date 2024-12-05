--zad 2
repl :: Eq a => [a] -> [(a, a)] -> [a]
repl l p  = foldr replace l p
  where
    replace (a, b) = map (\x -> if x == a then b else x)

--zad 1
rd :: Integer -> [Integer]
rd 1 = filter (\a -> sum [b | b <- [1..a], mod a b == 0] == 1 + a) [1..]
rd x = filter (\a -> sum [b | b <- [1..a], mod a b == 0] == x + a) [1..(x*x)]

--zad 2
-- repl :: Eq a => [a] -> [(a, a)] -> [a]
-- repl l r = foldl (\a -> map (\b -> ) a ) [] l

--zad 3

data Sdb a = Value a | Single a (Sdb a) | Double a (Sdb a) (Sdb a)

el :: Eq a => Sdb a -> a -> Bool
el (Value x) val = x == val
el (Single x t) val = x == val || el t val
el (Double x t1 t2) val = x == val || el t1 val || el t2 val

eq :: Eq a => Sdb a -> Sdb a -> Bool
eq (Value x1) (Value x2) = x1 == x2
eq (Single x1 t1) (Single x2 t2) = x1 == x2 && eq t1 t2
eq (Double x1 t1 t2) (Double x2 t3 t4) = x1 == x2 && ((eq t1 t3 && eq t2 t4) || (eq t1 t4 && eq t2 t3))
eq _ _ = False

bfs :: [Sdb a] -> [a]
bfs [] = []
bfs ((Value x):ts) = x : bfs ts
bfs ((Single x t):ts) = x : bfs (ts ++ [t])
bfs ((Double x t1 t2):ts) = x : bfs (ts ++ [t1, t2])
  
sdb2list :: Sdb a -> [a]
sdb2list x = bfs [x]

test = Double 5 (Value 4) (Value 3)
test2 = Double 5 (Value 3) (Value 4)
test3 = Double 5 (Single 3 (Value 2)) (Value 4)