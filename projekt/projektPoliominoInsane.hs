type Point = (Int, Int)
type Polyomino = [Point]    

sort :: Ord a => [a] -> [a]
sort = actualSort (>)

data SortedSegment a = MkSortedSegment [a] String


{-# INLINE actualSort #-}
actualSort :: forall a. (a -> a -> Bool) -> [a] -> [a]
actualSort (>.) ns
  | []        <- ns = []
  | [a]       <- ns = [a]
  -- | [a,b]     <- ns = s_data $ merge [(MkSortedSegment [a] "")] [(MkSortedSegment [b] "")]
  -- | [a,b,c]   <- ns = merge3 (MkSortedSegment [a] "") (MkSortedSegment [b] "") (MkSortedSegment [c] "")
  -- | [a,b,c,d] <- ns = merge4 [a] [b] [c] [d]
  | otherwise       = merge_all (sequences ns)
  where
    (-:) :: a -> SortedSegment a -> SortedSegment a
    x -: MkSortedSegment xs ei = MkSortedSegment (x : xs) ei

    sequences :: [a] -> [SortedSegment a]
    sequences [] = []
    sequences [x] = [MkSortedSegment [x] ":D"]
    sequences (a:b:xs)
      | a >. b  = descending b [a]  xs
      | otherwise = ascending  b (a:) xs

    descending :: a -> [a] -> [a] -> [SortedSegment a]
    descending a as (b:bs)
      | a >. b       = descending b (a:as) bs
    descending a as bs = MkSortedSegment (a:as) "lol" : sequences bs

    ascending a as (b:bs)
      | not (a >. b) = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs  = let !x = as [a]
                         in MkSortedSegment x ":D" : sequences bs

    merge_all :: [SortedSegment a] -> [a]
    merge_all [MkSortedSegment x _] = x
    merge_all xs = merge_all (reduce xs)

    reduce :: [SortedSegment a] -> [SortedSegment a]
    reduce []            = []
    reduce [a]           = [a]
    reduce [a,b]         = [merge a b]
    reduce [a,b,c]       = [merge3 a b c]
    reduce [a,b,c,d,e]   = [merge a b, merge3 c d e]
    reduce [a,b,c,d,e,f] = [merge3 a b c, merge3 d e f]
    reduce (a:b:c:d:xs)  = let !x = merge4 a b c d
                           in x : reduce xs

    merge as@(MkSortedSegment (a:as') aei) bs@(MkSortedSegment (b:bs') bei)
      | a >. b    = b -: merge as (MkSortedSegment bs' bei)
      | otherwise = a -: merge (MkSortedSegment as' aei) bs
    merge (MkSortedSegment [] _) bs   = bs
    merge as (MkSortedSegment [] _)   = as

    -- `merge3` is a manually fused version of `merge (merge as bs) cs`
    merge3 :: SortedSegment a -> SortedSegment a -> SortedSegment a -> SortedSegment a
    merge3 as@(MkSortedSegment (a:as') aei) bs@(MkSortedSegment (b:bs') bei) cs
      | a >. b    = merge3X b as  (MkSortedSegment bs' bei) cs
      | otherwise = merge3X a (MkSortedSegment as' aei) bs  cs
    merge3 (MkSortedSegment [] _) bs cs = merge bs cs
    merge3 as (MkSortedSegment [] _) cs = merge as cs

    merge3X x as bs cs@(MkSortedSegment (c:cs') cei)
      | x >. c    = c -: merge3X x as bs (MkSortedSegment cs' cei)
      | otherwise = x -: merge3 as bs cs
    merge3X x as bs (MkSortedSegment [] _) = x -: merge as bs

    merge3Y as@(MkSortedSegment (a:as') aei) y bs cs
      | a >. y    = y -: merge3 as bs cs
      | otherwise = a -: merge3Y (MkSortedSegment as' aei) y bs cs
    merge3Y (MkSortedSegment [] _) x bs cs = x -: merge bs cs

    -- `merge4 as bs cs ds` is (essentially) a manually fused version of
    -- `merge (merge as bs) (merge cs ds)`
    merge4 as@(MkSortedSegment (a:as') aei) bs@(MkSortedSegment (b:bs') bei) cs ds
      | a >. b    = merge4X b as (MkSortedSegment bs' bei) cs ds
      | otherwise = merge4X a (MkSortedSegment as' aei) bs cs ds
    merge4 (MkSortedSegment [] _) bs cs ds = merge3 bs cs ds
    merge4 as (MkSortedSegment [] _) cs ds = merge3 as cs ds

    merge4X x as bs cs@(MkSortedSegment (c:cs') cei) ds@(MkSortedSegment (d:ds') dei)
      | c >. d    = merge4XY x as bs d cs (MkSortedSegment ds' dei)
      | otherwise = merge4XY x as bs c (MkSortedSegment cs' cei) ds
    merge4X x as bs (MkSortedSegment [] _) ds = merge3X x as bs ds
    merge4X x as bs cs (MkSortedSegment [] _) = merge3X x as bs cs

    merge4Y as@(MkSortedSegment (a:as') aei) bs@(MkSortedSegment (b:bs') bei) y cs ds
      | a >. b    = merge4XY b as (MkSortedSegment bs' bei) y cs ds
      | otherwise = merge4XY a (MkSortedSegment as' aei) bs  y cs ds
    merge4Y as (MkSortedSegment [] _) y cs ds = merge3Y as y cs ds
    merge4Y (MkSortedSegment [] _) bs y cs ds = merge3Y bs y cs ds

    merge4XY x as bs y cs ds
      | x >. y  = y -: merge4X x as bs   cs ds
      | otherwise = x -: merge4Y   as bs y cs ds

group :: (Eq a) => [a] -> [[a]]
group [] = []
group (a:as) = (a:ys) : group zs
    where (ys,zs) = spanEq as
          spanEq [] = ([], [])
          spanEq (x:xs)
              | a == x = let (ps,qs) = spanEq xs
                          in (x:ps,qs)
              | otherwise = ([], x:xs)

removeDuplicates :: [Polyomino] -> [Polyomino]
removeDuplicates = map head . group . sort


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
generatePolyominoes x = removeDuplicates $ map (sort . normalize) (tryAllNeighbours (x - 1) [(1, 0), (0, 1), (-1, 0), (0, -1)] [(0, 0)] [(0, 0)])

countPolyominoes :: Int -> Int
countPolyominoes x = div (length (tryAllNeighbours (x - 1) [(1, 0), (0, 1), (-1, 0), (0, -1)] [(0, 0)] [(0, 0)])) x
