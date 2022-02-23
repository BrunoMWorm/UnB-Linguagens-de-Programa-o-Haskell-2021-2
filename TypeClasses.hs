-- Definição de uma CLASSE: conjunto de tipos
class MyEq t where
    (===) :: t -> t -> Bool

-- Definição de uma INSTÂNCIA de classe: definimos que Bool pertence
-- a classe 'MyEq'
instance MyEq Bool where
    True === True = True
    False === False = True
    _ === _ = False

instance (MyEq t, MyEq u) => MyEq (t,u) where
    (a,b) === (c,d) = a === c && b === d

-- Instância de um tipo: para um tipo '[a]'
-- [Int], [Char], [[Int]], [(Bool,Int)]

class Visible t where
    toString :: t -> String
    size :: t -> Int

-- Definindo que 'Char' é uma instância da classe 'Visible'
instance Visible Char where
    toString ch = [ch]
    size _ = 1

-- Definindo que 'Bool' é uma instância da classe 'Visible'
instance Visible Bool where
    toString True = "True"
    toString False = "False"
    size _ = 1

(>.>) :: (a -> b) -> (b -> c) -> a -> c
g >.> f = f . g

-- Definindo que [t] é uma instância de 'Visible' para t 'Visible'
-- Contexto "Visible t => ..."
instance Visible t => Visible [t] where
    toString = map toString >.> concat
    size = map size >.> sum -- 'sum' ou 'foldr (+) 0'

class (Ord t) => MyEnum t where
    enumFrom :: t -> [t]
    enumFromThen :: t -> t -> [t]
    enumFromTo :: t -> t -> [t]
    enumFromThenTo :: t -> t -> t -> [t]

-- Exemplo:
enumeracao = [1,3..15]
-- 'From' == 1
-- 'Then' == 3
-- 'To' == 15

-- show True: Bool -> String = "True"
-- read "True" :: Bool: String -> Bool = True