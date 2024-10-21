{-
f :: Double -> Double ->  Double
f x y = (x^2 + 2*x*y) / (y^2)

g = f 5
-}

f :: Int -> Int
f x = 4 * x

g :: Int -> Int
g y = 3 * y

sumaWartosci :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
sumaWartosci f g x y = f x + g y

sumaWartosci2 :: Int -> Int -> Int
sumaWartosci2 = sumaWartosci f g

sum3 = sumaWartosci (4*) (3*)

ocena :: Double -> String
ocena x 
    | x == 2.0 = "niezaliczone"
    | x == 5.0 = "brawo!"
    | otherwise = "wpisane masz " ++ show x