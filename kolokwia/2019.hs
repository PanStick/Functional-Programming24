-- --zad 1
-- sumDigits :: Int -> Int
-- sumDigits 0 = 0
-- sumDigits n = (n `mod` 10) + sumDigits (n `div` 10)

-- digitSumProcess :: Int -> Int
-- digitSumProcess n
--     | n < 10    = n
--     | otherwise = digitSumProcess (sumDigits n)

-- sevens :: Int -> [Int]
-- sevens n = take n [x | x <- [1..], digitSumProcess x == 7]

-- --zad 2
-- bp :: Int -> [String]
-- bp n
--     | n `mod` 4 /= 0 = []
--     | otherwise      = filter isBalancedPalindrom (generatePalindromes n)
--   where
--     isBalancedPalindrom str = aCount == bCount
--       where
--         aCount = length (filter (== 'a') str)
--         bCount = length (filter (== 'b') str)
--     generatePalindromes 0 = [""]
--     generatePalindromes 1 = []
--     generatePalindromes m = [[x] ++ middle ++ [x] | middle <- generatePalindromes (m - 2), x <- "ab"]

--zad 1

sumDigits :: Int -> Int
sumDigits val
  | val < 10 = val
  | otherwise = mod val 10 + sumDigits (div val 10)

calcSteps :: Int -> Int
calcSteps val
  | val < 10 = val
  | otherwise = calcSteps (sumDigits val)

sevens :: Int -> [Int]
sevens x = take x (filter (\a -> calcSteps a == 7) [1..])


--zad 2
isBalancedPalindrome :: String -> Bool
isBalancedPalindrome s = reverse s == s && length (filter (=='a') s) == div (length s) 2

stringGenerator :: Int -> [String] -> [String]
stringGenerator 1 strings = strings
stringGenerator i strings = stringGenerator (i-1) (map ('a' :) strings ++ map ('b' :) strings)

bp :: Int -> [String]
bp x
  | mod x 4 == 0 = filter isBalancedPalindrome (stringGenerator x ["a", "b"])
  | otherwise = []

--zad 3
type Mb a = ([a], [a])

dnp :: Mb a -> a -> Mb a
dnp (s, e) val = (val : s, e)

dnk :: Mb a -> a -> Mb a
dnk (s, e) val = (s, val : e)

mb2list :: Mb a -> [a]
mb2list (s, e) = s ++ reverse e

ull :: Mb a -> Mb a
ull (s, e) = ([], e)

upl :: Mb a -> Mb a
upl (s, e) = (s, [])

test = dnp (dnp ([], []) 3) 4
test2 = dnk (dnk (dnp (ull test) 2) 6) 1