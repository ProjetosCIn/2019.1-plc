data List t = Nil | Cons t (List t)
    deriving(Show)

data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving(Show)

-- exemplo toList (Cons 4 (Cons 3 Nil))
toList :: List t -> [t]
toList Nil = []
toList (Cons x xs) = x : toList xs

-- exemplo: fromList [4,3]
fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = (Cons x (fromList xs))

-- exemplo: depth(Node 5 NilT NilT)
depth :: Tree t -> Int
depth NilT = 0
depth (Node pai filho1 filho2) = 1 + max (depth filho1) (depth filho2)

-- transforma a árvore em lista
-- exemplo depth (Node 5 NilT NilT)
collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node pai filho1 filho2) = pai : collapse filho1  ++ collapse filho2

-- exemplo: mapTree (+21) (Node 5 NilT (Node 4 NilT NilT))
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node pai filho1 filho2) = Node (f pai) (mapTree f filho1) (mapTree f filho2)


charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

--encode :: String -> String
--encode _ = ""
--str2str (x:xs)
--   | x == xs !! 0 = :str2str(xs)
--    | otherwise = x:" ":str2str(xs)

repeatString :: Int -> Char -> String
repeatString 0 b = ""
repeatString n b = [b] ++ repeatString (n - 1) b 

-- example decode "3x2y1w2z"
decode :: String -> String
decode [] = []
decode (x:xs)
    | charToInt x >= 0 && charToInt x <= 9 = [x] ++ repeatString (charToInt x) (show xs !! 1) ++ decode xs
    | otherwise = decode xs