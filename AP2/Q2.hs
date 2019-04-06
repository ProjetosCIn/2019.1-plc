-- Questão 2 -  AP 2
-- [IF686EC] - 2019.1
-- autor: tasm2

data Tree t = Nilt |
            Node t (Tree t) (Tree t)
            deriving(Eq, Show)

isBST :: (Ord t) => Tree t -> Bool
isBST Nilt = True
isBST (Node t tmenor tmaior)
    | tmenor == Nilt && tmaior == Nilt = True 
    | tmaior /= Nilt && tmenor /= Nilt = maximum (treeToList tmenor) < t && t < minimum(treeToList tmaior) && isBST tmenor && isBST tmaior

treeToList :: Tree t -> [t]
treeToList Nilt = []
treeToList (Node t tleft tright) = t : treeToList tleft ++ treeToList tright

-- isBST (Node 6 (Node 2 Nilt (Node 5 () (Node 8 Nilt Nilt))) (Node 7 Nilt Nilt))