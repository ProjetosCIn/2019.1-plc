-- Questão 3 -  AP 2
-- [IF686EC] - 2019.1
-- autor: tasm2

data List t = Nil |
              Cons t (List t)
    deriving(Show)

mapList :: (t -> t) -> List t -> List t 
mapList f Nil = Nil
mapList f (Cons t x) = Cons (f t) (mapList f x)