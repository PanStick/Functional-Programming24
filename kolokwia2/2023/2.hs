-- dodajPunkty :: Integer -> [[Integer]] -> [Integer]
-- dodajPunkty dl (x:xs)
--     | null xs = x 
--     | otherwise = zipWith (+) x (dodajPunkty dl xs)

dodajPunkty :: Integer -> [[Integer]] -> [Integer]
-- dodajPunkty dl x = foldr (zipWith (+)) (repeat 0) x
-- dodajPunkty dl = foldr (zipWith (+)) (repeat 0)
-- dodajPunkty dl = foldr (zipWith (+)) (replicate (fromInteger dl) 0)
-- dodajPunkty dl = foldr (zipWith (+)) ((replicate . fromInteger) dl 0)
dodajPunkty dl x = foldr (zipWith (+)) (flip (replicate . fromInteger) 0 dl) x


-- dodajPunkty :: [[Integer]] -> [Integer]
-- dodajPunkty = foldr (zipWith (+)) (repeat 0)
