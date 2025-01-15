
countChar :: String -> [(Char, Int)]
countChar "" = []
countChar (x:xs) = (x, length (filter (== x) xs) + 1) : countChar (filter (/= x) xs)

main :: IO()
main = do
    input <- getLine
    if input /= "."
    then do
        print $ countChar input
        main
    else putStr ""