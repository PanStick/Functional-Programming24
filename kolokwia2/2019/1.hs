--a
div3 :: Int -> Bool
-- div3 x = (==0) (mod x 3)
-- div3 x = (==0) (flip mod 3 x)
div3 = (==0) . flip mod 3

--b
f :: [[Int]] -> Int
-- f x = length $ filter div3 (map (\a -> length $ filter div3 a) x)
-- f x = length $ filter div3 (map (length . filter div3) x)
-- f x = length $ (filter div3 . map (length . filter div3)) x
f  = length . filter div3 . map (length . filter div3)



