--zad 1
wszystkieCiagi :: Integer -> [[Integer]]
wszystkieCiagi n 
    | n==1 =[[1],[0]]
    | otherwise =map (1:) (wszystkieCiagi (n-1))  ++ map(0:) (wszystkieCiagi (n-1))


oddHelper :: [Integer] -> Bool
oddHelper xs= (length (filter (==1) xs) `mod` 2)==1 

oddbins :: Integer -> [[Integer]]
oddbins n= filter oddHelper (wszystkieCiagi n)