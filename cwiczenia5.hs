
--zad 1
data Student = Student {imie :: String,
                        nazwisko :: String,
                        nrAlbumu :: Int
                        }
janKowalski = Student {imie="Jan",
nazwisko="Kowalski", nrAlbumu=1234567}

instance Show Student where
    show (Student imie nazwisko nrAlbumu) =
        "imie: " ++ imie ++ "\n" ++
        "nazwisko: " ++ nazwisko ++ "\n" ++
        "nr albumu: " ++ show nrAlbumu

--zad 2
data Calkowite = Zero | Nast Calkowite | Pred Calkowite
    deriving Show

calkowiteToInteger :: Calkowite -> Integer
calkowiteToInteger Zero = 0
calkowiteToInteger (Nast x) = 1 + calkowiteToInteger x
calkowiteToInteger (Pred x) = calkowiteToInteger x - 1

integerToCalkowite :: Integer -> Calkowite
integerToCalkowite x
                    | x == 0 = Zero
                    | x > 0 = Nast (integerToCalkowite (x-1))
                    | x < 0 = Pred (integerToCalkowite (x+1))


data Tree a = Empty | Node a (Tree a) (Tree a)

--zad 3
allNodesEven :: Tree Integer -> Bool
allNodesEven Empty = True
allNodesEven (Node x y z) = even x && allNodesEven y && allNodesEven z

--zad 4
treePreorder :: Tree a -> [a]
treePreorder Empty = []
treePreorder (Node x y z) = x : treePreorder y ++ treePreorder z

treeInorder :: Tree a -> [a]
treeInorder Empty = []
treeInorder (Node x y z) = treeInorder y ++ [x] ++ treeInorder z

treePostorder :: Tree a -> [a]
treePostorder Empty = []
treePostorder (Node x y z) = treePostorder y ++ treePostorder z ++ [x]

--zad 5
data GaussianInteger = Integer :+ Integer

instance Show GaussianInteger where
    show (a :+ b) = show a ++ " + " ++ show b ++ "i"

instance Num GaussianInteger where
    (+) (a :+ b) (c :+ d) = (a + c) :+ (b + d)
    (*) (a :+ b) (c :+ d) = (a*c - b*d) :+ (a*d + b*c)
    abs (a :+ b) = a :+ b
    signum (a :+ b) = 1
    fromInteger a = a :+ 0
    negate (a :+ b) = (-a) :+ (-b)

--zad 6
data Matrix2x2 = Matrix2x2 Integer Integer Integer Integer

instance Show Matrix2x2 where
    show (Matrix2x2 a b c d) = "[" ++ show a ++ ", " ++ show b ++ "]\n" ++
                                "[" ++ show c ++ ", " ++ show d ++ "]"

sumMatrix2x2 :: Matrix2x2 -> Matrix2x2 -> Matrix2x2
sumMatrix2x2 (Matrix2x2 a1 b1 c1 d1) (Matrix2x2 a2 b2 c2 d2) =
    Matrix2x2 (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

multMatrix2x2 :: Matrix2x2 -> Matrix2x2 -> Matrix2x2
multMatrix2x2 (Matrix2x2 a1 b1 c1 d1) (Matrix2x2 a2 b2 c2 d2) =
    Matrix2x2 (a1*a2 + b1*c2) (a1*b2 + b1*d2) (c1*a2 + d1*c2) (c1*b2 + d1*d2)

test1 = Matrix2x2 1 2 3 4
test2 = Matrix2x2 5 6 7 8