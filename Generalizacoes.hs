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


-- Retorna true se a função 'f' é crescente no intervalo de 0 a 'n'
-- Crescente: ESTRITAMENTE CRESCENTE
isFunctionCrescent :: (Int -> Int) -> Int -> Bool
-- [f 0]
isFunctionCrescent _ 0 = True
isFunctionCrescent f n = f n > f (n - 1) && isFunctionCrescent f (n - 1)


-- Dada uma lista de inteiros, como calcular a soma dela?
sumList :: [Int] -> Int 
sumList [] = 0
sumList (a:as) = a + sumList as


-- OBSERVAÇÃO: Lembremos que:
-- t -> t -> t
-- ==
-- t -> (t -> t)
-- !=
-- (t -> t) -> t

myFold :: (t -> t -> t) -> [t] -> t
myFold f [a] = a
myFold f (a:as) = f a (myFold f as)

myFoldWithDefault _ a [] = a
myFoldWithDefault f a (e:es) = f e (myFoldWithDefault f a es)


sumListUsandoFold l = myFold (+) l
-- OU:
sumListUsandoFold' = myFold (+)

sumListUsandoFoldWithDefault l = myFoldWithDefault (+) 0 l
-- OU:
sumListUsandoFoldWithDefault' = myFoldWithDefault (+) 0

myFilter :: (t -> Bool) -> [t] -> [t]
myFilter p [] = []
myFilter p (a:as) | p a = a : myFilter p as
    | otherwise = myFilter p as

-- Exercícios do slide 18
elevaItensAoQuadrado = myMap quadrado
    where quadrado a = a*a

somaDosQuadrados l = myFold (+) (elevaItensAoQuadrado l)

mantemNaListaMaioresQueZero = myFilter maiorQueZero
    where maiorQueZero a = a > 0


-- Lembrando dos tipos definidos na aula de Listas.
type Person = String
type Book = String
type Database = [(Person, Book)]

-- Retorna os livros que uma pessoa pegou emprestado
books :: Database -> Person -> [Book]
books db per = map snd (filter isPer db)
    where isPer (p,b) = (p == per)

-- Remove da base de dados os empréstimos da pessoa 'p' com o livro 'b'
returnLoan :: Database -> Person
 -> Book -> Database
returnLoan db p b = filter notPB db
    where notPB pr = (pr /= (p,b))

-- Exercício interessante: remove duplicatas
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (a:as)
    | elem a as = removeDuplicates as
    | otherwise = a : removeDuplicates as 