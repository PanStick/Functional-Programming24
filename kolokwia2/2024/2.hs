

-- factorial :: Int -> Int
-- --factorial n = product (take n [1..])
-- factorial = product flip take [1..]

factorial :: Int -> Int
-- factorial n = product (take n [1..])
-- factorial n = product (flip take [1..] n)
factorial = product . flip take [1..]

liczbaEulera :: Int -> Double
-- liczbaEulera n = foldr (\i acc -> (1 / fromIntegral (factorial i)) + acc) 0 (take n [0..])
-- liczbaEulera n = foldr (\i acc -> (1 / fromIntegral (factorial i)) + acc) 0 (flip take [0..] n)
-- liczbaEulera n = foldr (\i acc -> (1 / (fromIntegral . factorial) i) + acc) 0 (flip take [0..] n)
-- liczbaEulera n = foldr (\i acc -> (((1 /) . fromIntegral . factorial) i) + acc) 0 (flip take [0..] n)
-- liczbaEulera n = foldr (\i acc -> (+) (((1 /) . fromIntegral . factorial) i) acc) 0 (flip take [0..] n)
-- liczbaEulera n = foldr (\i -> (+) (((1 /) . fromIntegral . factorial) i)) 0 (flip take [0..] n)
-- liczbaEulera n = foldr  ((+) . ((1 /) . fromIntegral . factorial))  0 (flip take [0..] n)
liczbaEulera = foldr ((+) . ((1 /) . fromIntegral . factorial)) 0 . flip take [0..]





-- liczbaEulera n = foldr (\i acc -> ((1/) . ((fromIntegral . factorial) i)) + acc) 0 (flip take [0..] n)

