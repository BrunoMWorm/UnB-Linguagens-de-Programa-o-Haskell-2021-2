answer :: Int
answer = 42

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int
square x = x * x

maxi :: Int -> Int -> Int
maxi n m  | n >= m = n
          | otherwise = m

addD :: Int -> Int -> Int
addD a b = 2 * (a+b)

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

-- Exercícios:
--fat :: Int -> Int
--fat n   | n == 0 = 1
--        | otherwise = n * fat (n - 1)

all4Equal a b c d = (a == b) && (b == c) && (c == d)

howManyEqual :: Int -> Int -> Int -> Int
-- howManyEqual 1 2 3 = 0
-- howManyEqual 1 1 3 = 2
-- howManyEqual 1 1 1 = 3
howManyEqual a b c    | allEqual a b c = 3
                      | (a == b) || (b == c) || (a == c) = 2
                      | otherwise = 0

sales :: Int -> Int
sales n = n + 3

totalSales :: Int -> Int
totalSales n  | n == 0 = sales 0
              | otherwise = totalSales (n-1) + sales n

maxSales :: Int -> Int
maxSales n  | n == 0 = sales 0
            | otherwise = maxi (maxSales (n-1)) (sales n)

-- Com 'Casamento De Padrões'

fat 0 = 1
fat n = n * fat(n-1)

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myOr :: Bool -> Bool -> Bool
myOr True x = True
myOr False x = x

myAnd :: Bool -> Bool -> Bool
myAnd False x = False
myAnd True x = x

myXor :: Bool -> Bool -> Bool
-- Caso quiséssemos manualmente fazer todos os casos:
-- myXor False False = True
-- myXor True False = True
-- myXor False True = True
-- myXor True True = False

myXor False x = x
myXor True x = myNot x

-- Para próxima aula: Exercício do slide 19
-- Instalação do ambiente Haskell