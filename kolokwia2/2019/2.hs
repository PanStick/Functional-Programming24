
main :: IO()
main = do
    input <- getLine
    if input /= "."
    then 
        do
        putStrLn $ reverse input
        main
    else putStr ""