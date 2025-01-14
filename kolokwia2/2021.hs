-- f :: (Enum b, Num b) => [b] -> [b]
-- -- f list = map (\(a,b) -> 3 * (a+b)) (zip list [1..10])
-- --[5, 3, 6] -> [(5, 1), (3, 2), (6, 3)] -> [18 = (3 * (5+1)), 15 = (3*(3+2)), 27 = 3*(6+3)]
-- -- f list = map (\x -> 3 * x) (zipWith (+) [1..10] list)
-- -- f list = map (3 *) (zipWith (+) [1..10] list)
-- -- f list = (map (3 *) . zipWith (+) [1..10]) list
-- f = map (3 *) . zipWith (+) [1..10]

f :: [Int] -> [Int]
-- f list = map (\(a,b) -> 3 * (a+b)) (zip list [1..10])
-- f list = map (\(a,b) -> 3 * (a+b)) (zip [1..10] list)
-- f list = map ((3*) . (\(a,b) -> (a+b))) (zip [1..10] list)
-- f list = map ((3*) . uncurry (+)) (zip [1..10] list)
-- f list = (map ((3*) . uncurry (+)) . (zip [1..10])) list
f = map ((3*) . uncurry (+)) . (zip [1..10])


-- f list = zipWith (\a b -> 3 * (a + b)) list [1..10]