-- dodajPunkty :: Integer -> [[Integer]] -> [Integer]
-- dodajPunkty dl (x:xs)
--     | null xs = x 
--     | otherwise = zipWith (+) x (dodajPunkty dl xs)

--nie zglaszac
dodajPunkty :: Integer -> [[Integer]] -> [Integer]
-- dodajPunkty dl x = foldr (zipWith (+)) (repeat 0) x
-- dodajPunkty dl = foldr (zipWith (+)) (repeat 0)
-- dodajPunkty dl = foldr (zipWith (+)) (replicate (fromInteger dl) 0)
-- dodajPunkty dl = foldr (zipWith (+)) ((replicate . fromInteger) dl 0)
-- dodajPunkty dl  = foldr (zipWith (+)) (flip (replicate . fromInteger) 0 dl)
dodajPunkty  = foldr (zipWith (+)) . flip (replicate . fromInteger) 0


-- dodajPunkty :: [[Integer]] -> [Integer]
-- dodajPunkty = foldr (zipWith (+)) (repeat 0)
