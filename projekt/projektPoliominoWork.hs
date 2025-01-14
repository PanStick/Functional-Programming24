type Point = (Int, Int)
type Polyomino = [Point]
data LinkedListPoint = LLP Point LinkedListPoint | Empty

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort x = let n = head x in
        sort (filter (< n) x) ++ [n] ++ sort (filter (> n) x)

normalize :: Polyomino -> Polyomino
normalize x = map (\(x, y) -> (x-dx, y-dy)) x
  where
    dx = minimum (map fst x)
    dy = minimum (map snd x)

notElem :: Point -> LinkedListPoint -> Bool
notElem x Empty = True
notElem x (LLP p llp)
    | x == p = False
    | otherwise = Main.notElem x llp

addLLP :: [Point] -> LinkedListPoint -> LinkedListPoint
addLLP xs llp = foldr LLP llp xs

checkPolyomino :: Int -> LinkedListPoint -> [Point] -> Polyomino -> [Polyomino]
checkPolyomino 0  _ _ child = [child]
checkPolyomino val llp restrictedPoints child =
    tryAllNeighbours val (addLLP (filter f neighbours) llp) restrictedPoints child
        where
            (x, y) = head child
            f a = a `Main.notElem` llp && a `Prelude.notElem` restrictedPoints
            neighbours = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- tryAllNeighbours :: Int -> LinkedListPoint -> [Point] -> Polyomino -> [Polyomino]
-- tryAllNeighbours _ Empty _ _ = []
-- tryAllNeighbours val (LLP x llp) restrictedPoints parent =
--      checkPolyomino (val-1) llp (x : restrictedPoints) (x : parent) ++ tryAllNeighbours val llp (x : restrictedPoints) parent

tryAllNeighbours :: Int -> LinkedListPoint -> [Point] -> Polyomino -> [Polyomino]
tryAllNeighbours _ Empty _ _ = []
tryAllNeighbours val (LLP x llp) restrictedPoints parent = foldr (\a b c)
     --checkPolyomino (val-1) llp (x : restrictedPoints) (x : parent) ++ tryAllNeighbours val llp (x : restrictedPoints) parent


generatePolyominoes :: Int -> [Polyomino]
generatePolyominoes x =  sort $ map (sort . normalize) (tryAllNeighbours x (LLP (0, 0) Empty) [] [])


countPolyominoes :: Int -> Int
countPolyominoes x = div (length (tryAllNeighbours x (LLP (0,0) Empty) [] [])) x
