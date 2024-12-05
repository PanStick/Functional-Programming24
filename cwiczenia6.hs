--zad 1
--f x = filter (\a -> a > 5) x
--f x = filter (>5) x
f = filter (>5)


g = map (flip (/) 5)

--zad 2
nonZero :: [Int] -> Int
--nonZero x = length (filter (/= 0) x) --(f (g (x)) = f . g)
nonZero = length . filter (/= 0)

--zad 3
-- m x list = map (\y -> y * x) list
-- m x = map (\y -> y * x)
-- m x = map (\y -> y (* x))
-- m x = map ((* x))
m = map . (*)

--zad 4
d :: [Double] -> Double -> [Double]
-- d list x = map (\y -> y / x) list
-- d list x = map (\y -> (/) y x) list
-- d list x = map (flip (/) x) list
-- d list x = flip map list (flip (/) x)
-- d list = (flip map list) . (flip (/))
-- d list = (.) (flip map list) (flip (/))
-- d list = flip (.) (flip (/)) (flip map list)
d = flip (.) (flip (/)) . flip map

--zad 5
wiekszeOd :: (Ord a) => [a] -> a -> [a]
-- wiekszeOd lista a = [x | x<-lista,x>a]
-- wiekszeOd lista a = filter (\x -> x > a) lista
-- wiekszeOd lista a = flip (\a lista -> (filter ((<) a)) lista) lista a
-- wiekszeOd lista a = flip (\a -> (filter . (<)) a) lista a
-- wiekszeOd lista a = flip (filter . (<)) lista a
wiekszeOd = flip (filter . (<))