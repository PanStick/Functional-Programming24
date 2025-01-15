
usun :: String -> String -> String 
usun x y = foldl (\a b -> if b `notElem` y then a ++ [b] else a) "" x