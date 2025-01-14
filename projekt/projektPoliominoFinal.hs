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

generatePolyominoes :: Int -> [Polyomino]
generatePolyominoes x = sort $ map (sort . normalize) (tryAllNeighbours x [(0,0)] [] [])

countPolyominoes :: Int -> Int
countPolyominoes x = div (length $ tryAllNeighbours x [(0,0)] [] []) x
