
genPrefixes :: [a] -> [[a]]
genPrefixes [] = [[]]
genPrefixes x = x : genPrefixes (init x)


iterateLength :: [a] -> [[a]]
iterateLength x = replicate (length x) x

-- prefixes :: [a] -> [[a]]
-- -- prefixes [] = [[]]
-- -- prefixes x = prefixes (init x) ++ [x]
-- -- prefixes x = foldl (\a b -> (head a ++ b) : a) [[]] (map (: []) x)
-- -- prefixes x = (foldl (\a b -> (head a ++ b) : a) [[]] . map (: [])) x
-- -- prefixes = foldl (\a b -> (head a ++ b) : a) [[]] . map (: [])
-- -- prefixes x = zipWith (\a b -> take a b) [1..] x
-- -- prefixes x = foldl (\a b -> b : a) [[]] (repeat x)
-- prefixes x = zipWith (curry (\(a, b) -> take a b)) [1..] (repeat x)

-- prefixes x = foldr (\a b -> [[a]] ++ b ) [[]] x

f :: a -> [a] -> [a]
f x a = x : a

prefixes :: [a] -> [[a]]
-- prefixes x = foldr (\x y -> [] : map (x :) y) [[]] x
-- prefixes = foldr (\x y -> [] : map (x :) y) [[]]
-- prefixes = foldr (\x y -> ((:) [] .  map (x :))y) [[]]
-- prefixes = foldr (\x y -> ((:) [] .  map (f x)) y) [[]]
-- prefixes = foldr (\x y -> ((:) [] . (map . f) x) y) [[]]
-- prefixes = foldr (\x y -> ((((:) [] .) . (map . f)) x) y) [[]]
prefixes = foldr ((([] :) .) . (map . f)) [[]]