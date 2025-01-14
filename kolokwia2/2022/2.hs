g :: [(Int, Int)] -> [Int]
-- g x = map (\(x, y) -> x - y) x
-- g = map (\(x, y) -> x - y)
g = map (uncurry (-))

h :: [(Int, Int)] -> Int
-- h x = foldr (*) 1 (g x)
h = foldr (*) 1 . g

f :: [(Int, Int)] -> Int -> Int
-- f x u = (h x) * u
-- f x u = (*) (h x) u
-- f x = (*) (h x)
-- f x = ((*) . h) x
f = (*) . h