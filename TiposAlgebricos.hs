data MyBool = MyTrue | MyFalse

data Estacao = Inverno | Verao | Outono | Primavera
data Temp = Frio | Quente

clima :: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

type Name = String
type Age = Int

data People = Person Name Age

showPerson :: People -> String
showPerson (Person n a) = n ++ " -- " ++ show a

data Shape = Circle Float | Rectangle Float Float

isRound :: Shape -> MyBool
isRound (Circle _) = MyTrue
isRound (Rectangle _ _) = MyFalse

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle h w) = h * w

data Expr =
    Lit Int |
    Add Expr Expr |
    Sub Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

data Pairs t = Pair t t

data List t = Nil | Cons t (List t)

meuLength :: [a] -> Int
meuLength [] = 0
meuLength (_:as) = 1 + meuLength as

meuLength' :: List a -> Int
meuLength' Nil = 0
meuLength' (Cons _ as) = 1 + meuLength' as

data Tree t = NilT | Node t (Tree t) (Tree t)