times2 :: Int -> Int
times2 n = 2 * n

sqr :: Int -> Int
sqr n = n * n

myMap :: (t -> u) -> [t] -> [u]
myMap f [] = []
myMap f (a:as) = f a : myMap f as

doubleList xs = myMap times2 xs
sqrList xs = myMap sqr xs


-- Como implementar o myMap usando compreensão de listas?
myMapLC :: (t -> u) -> [t] -> [u]
myMapLC f l = [f a | a <- l]

-- O máximo valor da aplicação de uma função em elementos de 0 a n.
maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = max (maxFun f (n-1)) (f n)

-- E usando map?
maxFunMap :: (Int -> Int) -> Int -> Int
maxFunMap f n = maximum $ map f [0..n]

-- Retorna true se existe algum inteiro i de 0 a n tal que f i == 0
zeroInRange :: (Int -> Int) -> Int -> Bool
zeroInRange f 0 = f 0 == 0
zeroInRange f n = zeroInRange f (n-1) || (f n == 0)
