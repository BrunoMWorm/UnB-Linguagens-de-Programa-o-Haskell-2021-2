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

-- Obs: zero é uma semana válida, correspondente à primeira semana
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

weeklySalesEqualToS :: Int -> Int -> Int
weeklySalesEqualToS s 0
        | s == sales 0 = 1
        | otherwise = 0
weeklySalesEqualToS s n
        | s == sales n = 1 + weeklySalesEqualToS s (n - 1)
        | otherwise = weeklySalesEqualToS s (n - 1)

makeSpaces :: Int -> String
makeSpaces 0 = ""
-- Usando concatenação de strings, o desempenho pode ser afeto
-- makeSpaces n = " " ++ makeSpaces (n - 1)
-- Param melhor
makeSpaces n = ' ' : makeSpaces (n - 1)

pushRight :: Int -> String -> String
pushRight n str = str ++ makeSpaces n

averageSales :: Int -> Float
averageSales n = fromIntegral (totalSales n) / fromIntegral (n + 1)

addPair :: (Int,Int) -> Int
addPair (x,y) = x+y

type Name = String
type Age = Int
type Phone = Int
type Person = (Name, Age, Phone)

name :: Person -> Name
-- Esse underline: significa que não nos importamos com o valor que está ali
name (n,_,_) = n


sumSquares :: Int -> Int -> Int

-- Com where
sumSquares x y = sq x + sq y
        where sq z = z * z

-- Com let
sumSquares2 x y =    let    sqX = x * x
                            sqY = y * y
                     in     sqX + sqY

-- Para próxima aula: exercício do slide 33,
oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b/(2.0*a)

twoRoots :: Float -> Float -> Float -> (Float, Float)
twoRoots a b c = (d-e, d+e)
    where   d = -b/(2.0*a)
            e = sqrt(b^2-4.0*a*c)/(2.0*a)

roots :: Float -> Float -> Float -> String
roots a b c     | b^2 == 4.0*a*c = show (oneRoot a b c)
                | b^2 > 4.0*a*c = show f ++ " " ++ show s
                | otherwise = "no roots"
    where (f,s) = twoRoots a b c

-- X² - 5x + 6 = 0 ==> "2.0 3.0"
-- X² - 2x + 1 = 0 ==> "1.0"
-- X² - 2x + 4 = 0 ==> "no roots"