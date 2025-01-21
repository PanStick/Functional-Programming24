type Point = (Int, Int)
type Polyomino = [Point]

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (> x) xs)

normalize :: Polyomino -> Polyomino
normalize x = map (\(x, y) -> (x-dx, y-dy)) x
  where
    dx = minimum (map fst x)
    dy = minimum (map snd x)

earlierInGrid :: Point -> Point -> Bool
earlierInGrid (x1, y1) (x2, y2)
  | y1 == y2 = x1 > x2
  | y1 < y2 = True
  | y1 > y2 = False

sortGrid :: [Point] -> [Point]
sortGrid [] = []
sortGrid (x:xs) = sortGrid (filter (earlierInGrid x) xs) ++ [x] ++ sortGrid (filter (not . earlierInGrid x) xs)

addNewLines :: Int -> String -> String
addNewLines _ [] = "\n"
addNewLines n str = take n str ++ "\n" ++ addNewLines n (drop n str)

generateGrid :: Int -> Int -> [Point]
generateGrid x y = [(x, y) | x <- [0..x], y <- [0..y]]

parsePolyomino :: Polyomino -> String
parsePolyomino x = addNewLines (dx+1) (map (\a -> if a `elem` x then '*' else ' ') (sortGrid $ generateGrid dx dy))
  where
    dx = maximum (map fst x)  
    dy = maximum (map snd x)

checkPolyomino :: Int -> [Point] -> [Point] -> Polyomino -> [Polyomino]
checkPolyomino 0  _ _ child = [child]
checkPolyomino val untriedPoints restrictedPoints child =
    tryAllNeighbours val (filter f neighbours ++ untriedPoints) restrictedPoints child
        where
            (x, y) = head child
            f a = a `notElem` untriedPoints && a `notElem` restrictedPoints
            neighbours = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

tryAllNeighbours :: Int -> [Point] -> [Point] -> Polyomino -> [Polyomino]
tryAllNeighbours _ [] _ _ = []
tryAllNeighbours val (x:xs) restrictedPoints parent =
     checkPolyomino (val-1) xs (x : restrictedPoints) (x : parent) ++ tryAllNeighbours val xs (x : restrictedPoints) parent

--sort wykonany na liscie poliomin uzywam jako usuniecie powtorek
generatePolyominoes :: Int -> String
generatePolyominoes x = concatMap parsePolyomino (sort $ map (sort . normalize) (tryAllNeighbours x [(0,0)] [] []))

countPolyominoes :: Int -> Int
countPolyominoes x = div (length $ tryAllNeighbours x [(0,0)] [] []) x