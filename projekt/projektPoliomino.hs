
type Cell = (Int, Int)
type Polyomino = [Cell]

-- addTopNeighbour :: Int -> Polyomino -> Polyomino
-- addTopNeighbour _ _ = []

-- addLeftNeighbour :: Int -> Cell -> Polyomino -> Polyomino
-- addLeftNeighbour size (x, y) poly
--   | (x+1, y) `notElem` poly = generate (size-1) (x+1, y) ((x+1, y):poly)


-- addNeighbours size (x, y) poly
--   | (x, y+1) `notElem` poly = generate (size-1) (x, y+1) ((x, y+1): poly)
-- --addLeftNeighbour size poly =  addTopNeighbour size poly


addCell :: Int -> Cell -> Polyomino -> [Polyomino]
addCell size cell poly =
  if cell `elem` poly
    then []
    else generate (size-1) cell (cell:poly)

generate :: Int -> Cell -> Polyomino -> [Polyomino]
generate _ _ [] = []
generate 1 _ poly = [poly]
generate size (x, y) poly =
  addCell size (x+1, y) poly ++
  addCell size (x, y+1) poly ++
  addCell size (x-1, y) poly ++
  addCell size (x, y-1) poly ++
  if (x, y) == (0, 0)
    then []
  else generate size (findPrev (x, y) poly) poly

findPrev :: Cell -> Polyomino -> Cell
findPrev cell (c1:c2:xs)
  | c1 == cell = c2
  | otherwise = findPrev cell (c2:xs)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

group :: Eq a => [a] -> [[a]]
group [] = []  -- Base case: empty list results in an empty list of groups
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . quickSort

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort x = let n = head x in
        quickSort (filter (< n) x) ++ [n] ++ quickSort (filter (> n) x)

normalize :: Polyomino -> Polyomino
normalize x = map (\(x, y) -> (x-dx, y-dy)) x
  where
    dx = minimum (map fst x)
    dy = minimum (map snd x)

generatePolyominos :: Int -> [Polyomino]
generatePolyominos x = rmdups (map (normalize . quickSort) (generate x (0, 0) [(0, 0)]))

delete :: (Eq a) => a -> [a] -> [a]
delete val (x:xs) =
    if val == x
        then xs
        else x : delete val xs

tryAdd :: (Eq a) => a -> [a] -> [a]
tryAdd x xs =
    if x `elem` xs
        then xs
        else x : xs

checkPolyomino :: Int -> [Cell] -> [Cell] -> Polyomino -> [Polyomino]
checkPolyomino 1 _ _ child = [child]
checkPolyomino val untriedPoints restrictedPoints child =
    tryAllNeighbours val (filter (`notElem` restrictedPoints) [(x+1, y), (x, y+1), (x-1, y), (x, y-1)] ++ untriedPoints) restrictedPoints child
        where (x, y) = head child

tryAllNeighbours :: Int -> [Cell] -> [Cell] -> Polyomino -> [Polyomino]
tryAllNeighbours _ [] _ _ = []
tryAllNeighbours val (x:xs) restrictedPoints parent =
    checkPolyomino (val-1) xs (tryAdd x restrictedPoints) (x : parent) ++
    tryAllNeighbours val xs (tryAdd x restrictedPoints) parent

generate2 :: Int -> [Polyomino]
generate2 x = rmdups (map (normalize . quickSort) (tryAllNeighbours x [(1, 0), (0, 1), (-1, 0), (0, -1)] [(0, 0)] [(0, 0)]))

check :: Int -> [Polyomino]
check x = filter (\a -> a `notElem` g1) g2
  where
    g1 = generatePolyominos x
    g2 = generate2 x

-- generatePolyominos2 :: Int -> Polyomino
-- generatePolyominos2 x = removeDuplicates (map (normalize . quickSort) (generate x (0, 0) [(0, 0)]))

-- generatePolyominos :: Int -> [Polyomino]
-- generatePolyominos x = map (normalize . quickSort) (generate x (0, 0) [(0, 0)])


-- import Data.List (nub, sort)
-- import Data.Set (Set)
-- import qualified Data.Set as Set

-- type Cell = (Int, Int)
-- type Polyomino = [Cell]

-- -- Normalize a polyomino by translating it to the top-left corner and sorting its cells
-- normalize :: Polyomino -> Polyomino
-- normalize poly = sort [(x - minX, y - minY) | (x, y) <- poly]
--   where
--     minX = minimum (map fst poly)
--     minY = minimum (map snd poly)

-- -- Generate all distinct polyominoes of a given size
-- generatePolyominoes :: Int -> [Polyomino]
-- generatePolyominoes n = nub [normalize p | p <- generate n [(0, 0)] [(0, 0)]]
--   where
--     -- Recursive function to generate polyominoes
--     generate :: Int -> Polyomino -> [Cell] -> [Polyomino]
--     generate 1 polyent _ = [polyent]
--     generate size polyent frontier = concatMap expand frontier
--       where
--         -- Expand by adding a new cell to the polyent polyomino
--         expand :: Cell -> [Polyomino]
--         expand cell =
--           generate (size - 1) (cell : polyent) newFrontier
--           where
--             newFrontier = nub (neighbors cell ++ frontier) \\ (cell : polyent)

--     -- Find all neighbors of a cell
--     neighbors :: Cell -> [Cell]
--     neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]

--     -- Exclude cells already in the polyomino
--     (\\) :: (Eq a) => [a] -> [a] -> [a]
--     xs \\ ys = filter (`notElem` ys) xs