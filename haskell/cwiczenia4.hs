--zad1
foo = foldr (-) 0 [1, 2, 3]
bar = foldl (-) 0 [1, 2, 3]

--zad2
reverseFoldR :: [a] -> [a]
reverseFoldR = foldr (\x y -> y ++ [x]) []

reverseFoldL :: [a] -> [a]
reverseFoldL = foldl (\x y -> y : x) []

--zad3
mapFoldL :: (a -> a) -> [a] -> [a]
mapFoldL f = foldl (\x y -> x ++ [f y]) []

mapFoldR :: (a -> a) -> [a] -> [a]
mapFoldR f = foldr (\x y -> f x : y) []

--zad4

-- doDziesietnego :: Int -> [Int] -> Int
-- doDziesietnego x xs = fst (foldr (\a (b, c) -> (b + a * c, c * x)) (0, 1) xs)

doDziesietnego :: Int -> [Int] -> Int
doDziesietnego a = foldl (\x y -> x * a + y) 0 

--zad 5
help :: (a -> Bool) -> (a -> Bool) -> (a,a) -> Int
help w1 w2 (x,y) = if w1 x && w2 y then 1 else 0

ktora :: (Int, Int) -> (Int, Int,Int,Int)
ktora x = (help (0<=) (0<=) x , help (0>=) (0 <=) x, help (0>=)(0>=) x ,help (0<=) (0>=) x)

zliczacz :: [(Int,Int,Int,Int)] -> (Int,Int,Int,Int)
zliczacz = foldr (\(x1,x2,x3,x4) (y1,y2,y3,y4) -> (x1+y1,x2+y2,x3+y3,x4+y4) ) (0,0,0,0)

max :: (Int,Int,Int,Int) -> Int
max (x,y,z,t) | x >= maximum [x,y,z,t] = 1 | y >= maximum [x,y,z,t] = 2 | z >= maximum [x,y,z,t] = 3 | otherwise = 4

ktoraCwiartka :: [(Int,Int)] -> Int
ktoraCwiartka = Main.max.zliczacz.map ktora

--zad6
dlaKazdego :: (a -> Bool) -> [a] -> Bool
dlaKazdego f = foldl (\x y -> x && f y) True

istnieje :: (a -> Bool) -> [a] -> Bool
istnieje f = foldl (\x y -> x || f y) False