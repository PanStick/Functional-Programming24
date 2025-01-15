
drawBase :: Int -> IO()
drawBase n = putStrLn $ replicate (n-1) ' ' ++ "||" ++ replicate (n-1) ' '

drawBottom :: Int -> IO()
drawBottom n = putStrLn $ replicate (2*n) '^'

draw :: Int -> IO()
draw n = putStr $ concatMap (\a -> replicate (n-a-1) ' ' ++ '/' : replicate (2*a) ' ' ++ "\\\n") [0..(n-1)]

main :: IO()
main = do
    putStrLn "Wprowadz wysokosc choinki"
    input <- getLine
    let n = read input :: Int
    draw n
    drawBottom n
    drawBase n