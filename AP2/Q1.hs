-- Questão 1 -  AP 2
-- [IF686EC] - 2019.1
-- autor: tasm2

data Ops = SUM | SUB | MUL

data IntTree = Nilt Int |
               Node Ops IntTree IntTree

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node SUM x y) = evalTree x + evalTree y
evalTree (Node SUB x y) = evalTree x - evalTree y
evalTree (Node MUL x y) = evalTree x * evalTree y 




