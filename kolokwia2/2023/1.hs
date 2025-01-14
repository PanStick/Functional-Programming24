
calcRes :: String -> Char -> Int -> IO()
calcRes (x:xs) y r
    | x == y = game xs r
    | x == 'P' && y == 'K' = game xs (r+1)
    | x == 'P' && y == 'N' = game xs (r-1)
    | x == 'K' && y == 'N' = game xs (r+1)
    | x == 'K' && y == 'P' = game xs (r-1)
    | x == 'N' && y == 'P' = game xs (r+1)
    | x == 'N' && y == 'K' = game xs (r-1)


game :: String -> Int -> IO()
game "" r = do
  putStrLn ("Ostateczny wynik: " ++ show r)
  return ()
game x r = do
    putStrLn  ("Aktualny wynik: " ++ show r)
    putStrLn "Wprowadz ruch"
    input <- getLine
    calcRes x (head input) r

main :: IO()
main = do
    input <- getLine
    game input 0
    