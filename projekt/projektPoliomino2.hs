type Point = (Int, Int)
type Polyomino = [Point]

data LinkedListPoint = LinkedListPoint Point LinkedListPoint

tryAdd :: (Eq a) => a -> [a] -> [a]
tryAdd x xs =
    if x `elem` xs
        then xs
        else x : xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort x = let n = min (head x) (last x) in
        quickSort (filter (< n) x) ++ [n] ++ quickSort (filter (> n) x)

normalize :: Polyomino -> Polyomino
normalize x = map (\(x, y) -> (x-dx, y-dy)) x
  where
    dx = minimum (map fst x)
    dy = minimum (map snd x)

checkPolyomino :: Int -> [Point] -> [Point] -> Polyomino -> [Polyomino]
checkPolyomino 0 _ _ child = [child]
checkPolyomino val untriedPoints restrictedPoints child =
    -- tryAllNeighbours val (filter (`notElem` restrictedPoints) [(x+1, y), (x, y+1), (x-1, y), (x, y-1)] ++ untriedPoints) restrictedPoints child
    tryAllNeighbours val (filter f neighbours ++ untriedPoints) restrictedPoints child
        where
            (x, y) = head child
            f a = a `notElem` restrictedPoints && a `notElem` untriedPoints
            neighbours = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

-- tryAllNeighbours :: Int -> [Point] -> [Point] -> Polyomino -> [Polyomino]
-- tryAllNeighbours _ [] _ _ = []
-- --(x:xs) = untriedPoints
-- tryAllNeighbours val (x:xs) restrictedPoints parent =
--      checkPolyomino (val-1) xs (x : restrictedPoints) (x : parent) ++ tryAllNeighbours val xs (x : restrictedPoints) parent
--     -- tryAllNeighbours val xs (tryAdd x restrictedPoints) parent ++
--     -- if x `notElem` restrictedPoints
--     --     then checkPolyomino (val-1) xs (x : restrictedPoints) (x : parent)
--     --     else []
--     -- if x `notElem` restrictedPoints
--     --     then checkPolyomino (val-1) xs (x:restrictedPoints) (x : parent) ++
--     --         tryAllNeighbours val xs (tryAdd x restrictedPoints) parent
--     --     else
--     --         tryAllNeighbours val xs (tryAdd x restrictedPoints) parent

tryAllNeighbours :: Int -> [Point] -> [Point] -> Polyomino -> [Polyomino]
tryAllNeighbours 0 _ _ poly = [poly]
tryAllNeighbours _ [] _ _ = []
tryAllNeighbours val (a:xs) restrictedPoints poly =
         tryAllNeighbours (val - 1) (filter f neighbours ++ xs) newRestrictedPoints (a : poly) ++ tryAllNeighbours val xs newRestrictedPoints poly
        where
            (x, y) = a
            newRestrictedPoints = a : restrictedPoints
            f n = n `notElem` newRestrictedPoints && n `notElem` xs
            neighbours = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)] 


isSmaller :: Polyomino -> Polyomino -> Bool
isSmaller [] _ = False
isSmaller (x:xs) (y:ys)
    | x < y = True
    | x > y = False
    | otherwise = isSmaller xs ys

isBigger :: Polyomino -> Polyomino -> Bool
isBigger [] _ = False
isBigger (x:xs) (y:ys)
    | x > y = True
    | x < y = False
    | otherwise = isBigger xs ys

{-# INLINEABLE sort #-} -- allows specialization for the ord instance
sort = actualSort (>)

{-# INLINEABLE sortBy #-}
sortBy cmp = actualSort (\x y -> cmp x y == GT)

actualSort :: (a -> a -> Bool) -> [a] -> [a]
actualSort gt ns
  | []        <- ns = []
  | [a]       <- ns = [a]
  | [a,b]     <- ns = merge [a] [b]
  | [a,b,c]   <- ns = merge3 [a] [b] [c]
  | [a,b,c,d] <- ns = merge4 [a] [b] [c] [d]
  | otherwise       = merge_all (sequences ns)
  where
    sequences (a:b:xs)
      | a `gt` b  = descending b [a]  xs
      | otherwise = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `gt` b       = descending b (a:as) bs
    descending a as bs = (a:as): sequences bs

    ascending a as (b:bs)
      | not (a `gt` b) = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs  = let !x = as [a]
                         in x : sequences bs

    merge_all [x] = x
    merge_all xs  = merge_all (reduce_once xs)

    reduce_once []            = []
    reduce_once [a]           = [a]
    reduce_once [a,b]         = [merge a b]
    reduce_once [a,b,c]       = [merge3 a b c]
    reduce_once [a,b,c,d,e]   = [merge a b, merge3 c d e]
    reduce_once [a,b,c,d,e,f] = [merge3 a b c, merge3 d e f]
    reduce_once (a:b:c:d:xs)  = let !x = merge4 a b c d
                                in x : reduce_once xs

    merge as@(a:as') bs@(b:bs')
      | a `gt` b  = b : merge as  bs'
      | otherwise = a : merge as' bs
    merge [] bs   = bs
    merge as []   = as

    -- `merge3` is a manually fused version of `merge (merge as bs) cs`
    merge3 as@(a:as') bs@(b:bs') cs
      | a `gt` b  = merge3X b as  bs' cs
      | otherwise = merge3X a as' bs  cs
    merge3 [] bs cs = merge bs cs
    merge3 as [] cs = merge as cs

    merge3X x as bs cs@(c:cs')
      | x `gt` c  = c : merge3X x as bs cs'
      | otherwise = x : merge3    as bs cs
    merge3X x as bs [] = x : merge as bs

    merge3Y as@(a:as') y bs cs
      | a `gt` y  = y : merge3  as    bs cs
      | otherwise = a : merge3Y as' y bs cs
    merge3Y [] x bs cs = x : merge bs cs

    -- `merge4 as bs cs ds` is (essentially) a manually fused version of
    -- `merge (merge as bs) (merge cs ds)`
    merge4 as@(a:as') bs@(b:bs') cs ds
      | a `gt` b  = merge4X b as  bs' cs ds
      | otherwise = merge4X a as' bs  cs ds
    merge4 [] bs cs ds = merge3 bs cs ds
    merge4 as [] cs ds = merge3 as cs ds

    merge4X x as bs cs@(c:cs') ds@(d:ds')
      | c `gt` d  = merge4XY x as bs d cs  ds'
      | otherwise = merge4XY x as bs c cs' ds
    merge4X x as bs [] ds = merge3X x as bs ds
    merge4X x as bs cs [] = merge3X x as bs cs

    merge4Y as@(a:as') bs@(b:bs') y cs ds
      | a `gt` b  = merge4XY b as  bs' y cs ds
      | otherwise = merge4XY a as' bs  y cs ds
    merge4Y as [] y cs ds = merge3Y as y cs ds
    merge4Y [] bs y cs ds = merge3Y bs y cs ds

    merge4XY x as bs y cs ds
      | x `gt` y  = y : merge4X x as bs   cs ds
      | otherwise = x : merge4Y   as bs y cs ds

group :: (Eq a) => [a] -> [[a]]
group [] = []
group (a:as) = (a:ys) : Main.group zs
    where (ys,zs) = spanEq as
          spanEq [] = ([], [])
          spanEq (x:xs)
              | a == x = let (ps,qs) = spanEq xs
                          in (x:ps,qs)
              | otherwise = ([], x:xs)

removeDuplicates :: [Polyomino] -> [Polyomino]
removeDuplicates = map head . group . sort

-- removeDuplicates :: [Polyomino] -> [Polyomino] -> [Polyomino]
-- removeDuplicates res [] = res
-- removeDuplicates res (x:xs) =
--     let newx = map (\(a, b) -> (a-minimum (map fst x), b-minimum (map snd x))) x in
--     if newx `notElem` res then
--         removeDuplicates (newx : res) xs
--     else
--         removeDuplicates res xs


generatePolyominoes :: Int -> [Polyomino]
generatePolyominoes x = removeDuplicates (map (quickSort . normalize) (tryAllNeighbours (x-1) [(1, 0), (0, 1), (-1, 0), (0, -1)] [(0, 0)] [(0, 0)]))

countPolyomines :: Int -> Int
countPolyomines x = div (length (tryAllNeighbours (x-1) [(1, 0), (0, 1), (-1, 0), (0, -1)] [(0, 0)] [(0, 0)])) x


-- dividePolyominos :: [Polyomino] -> Int -> [[Polyomino]]
-- dividePolyominos xs bucketSize =
--     take bucketSize xs :
--     dividePolyominos (drop bucketSize xs) bucketSize

-- findFaster :: Polyomino -> [[Polyomino]] -> Bool
-- findFaster _ [] = False
-- findFaster val (x:y:xs) =
--     if val < head y
--         then val `elem` x
--     else findFaster val (y:xs)
-- findFaster val (x:xs) = val `elem` x

