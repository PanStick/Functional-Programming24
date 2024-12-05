-- --zad1
-- dlugosc :: String -> Int
-- dlugosc s
--   | length s < 2 || all (== 'a') s = 0
--   | otherwise = 1 + dlugosc (transform s)

-- rule :: Char -> Char -> String
-- rule 'a' 'b' = "a"
-- rule 'b' 'a' = "b"
-- rule 'b' 'b' = "a"
-- rule 'a' 'a' = "aaa"
-- rule x y = [x, y]

-- transform :: String -> String
-- transform [] = []
-- transform [x] = [x]
-- transform (x:y:xs) = rule x y ++ transform xs

-- --zad 2
-- val :: Integer -> Integer -> Integer
-- val a b = last (takeWhile (\x -> mod a (b^x) == 0) [0..])

-- g :: Integer -> Integer -> [Integer]
-- g k v | (k > 1) && (v >= 0) = [n | n <- [2..], val n k == v]
--       | otherwise = []

--zad 3
data Klos a = Empty | Pocz a (Klos a) | Kon (Klos a) a

wnpk :: Klos a -> a -> Klos a
wnpk kl a = Pocz a kl

wnkk :: Klos a -> a -> Klos a
wnkk = Kon

konwK :: [a] -> [a] -> Klos a -> [a]
konwK pocz kon Empty = reverse pocz ++ kon
konwK pocz kon (Pocz a klos) = konwK (a : pocz) kon klos
konwK pocz kon (Kon klos a) = konwK pocz (a : kon) klos

k2list :: Klos a -> [a]
k2list = konwK [] []

test = Pocz 3 (Kon (Pocz 2 (Kon Empty 3)) 4)

--zad 1
transform :: String -> String
transform (a:b:x)
  | a == 'a' && b == 'b' = 'a' : transform x
  | a == 'b' && b == 'a' = 'b' : transform x
  | a == 'b' && b == 'b' = 'a' : transform x
  | a == 'a' && b == 'a' = "aaa" ++ transform x
  | otherwise = a : b : transform x
transform x = x


dlugosc :: String -> Int
dlugosc x =
    if elem 'b' x && length x > 1
      then 1 + dlugosc (transform x) else 0

--zad 2


val :: Integer -> Integer -> Integer
val a b = fromIntegral (length (
  filter (\x -> mod a x == 0) (
    takeWhile (<a) (iterate (*b) b))))

g :: Integer -> Integer -> [Integer]
g k v = filter (\n -> val n k == v) [(k+1)..]
