displayList :: [Integer] -> IO ()
displayList [] = putStr ""
displayList (x:[]) = print x
displayList (x:xs) = do
    putStr (show x)
    putStr ","
    displayList xs
 
fib :: Int -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
main = do
    putStrLn "Podaj liczbe: "
    liczba<-getLine
    let n = read liczba :: Int
    displayList (map fib [1..n])