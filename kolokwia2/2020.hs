--zad 1
ce :: [[Int]] -> [Int]
-- ce x = concat (filter (\a -> even (sum a)) x)
-- ce x = concat (filter (even . sum) x)
-- ce x = (concat . filter (even . sum)) x
ce = concat . filter (even . sum)

