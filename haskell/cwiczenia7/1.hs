--zad 1
main = do
    input <- getLine
    let val = read input
    print $ product [1..val]