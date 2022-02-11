
-- Definindo a composição para frente
(>.>) :: (a -> b) -> (b -> c) -> a -> c
g >.> f = f . g

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = f >.> iter (n-1) f
-- iter 1 f = f >.> iter 0 f = f >.> id = f
-- iter 2 f = f >.> iter 1 f = f >.> f

twice :: (t -> t) -> (t -> t)
twice f = f . f

addNum :: Int -> (Int -> Int)
addNum n = h
    where h m = n + m

addNumLambda :: Int -> (Int -> Int)
addNumLambda n = \m -> n + m


invertParameters f x y = f y x
-- E usando funções anônimas (lambdas?)
invertParametersLambda f = \x y -> f y x

filtrado = filter (\x -> x > 4) [-1,-2,0,1,2,3]

multiply :: Int -> Int -> Int
multiply a b = a*b
dobraALista = map (multiply 2)

divisao = (/)
dividirPorN n = \m -> divisao m n
dividirPor2 = dividirPorN 2
quatro = dividirPor2 8 -- = 4

-- Pense que 'multiply' é algo que aceita 4 e retorna algo que aceita 5 e retorna 20.
-- Onde 'algo' é nada mais que uma função...
-- multiply 4 5 == (multiply 4) 5

soma2 = (+2)
dividePor2 = (/2)
oitoDividindoAlgo = (8/)

adiciona3NaLista = (3:)

adicionaNewLine = (++ ['\n'])

adiciona1EmTodosElementosEFiltraPositivos = map (+1) >.> filter (>0)

dobraAListaUsandoSecao = map (*2)

soma = (+)
addNumUsandoAplicacaoParcial n = soma n

-- CURRYING, UNCURRYING e FLIP

-- CURRYING:
myCurry :: ((t,u) -> v) -> (t -> u -> v)
myCurry g a b = g (a,b)
-- minhaFuncao(a,b) = a + b
-- Transforma isso em:
-- minhaFuncao a b = a + b
-- EXEMPLO:
minhaSoma(a,b) = a + b
-- minhaSoma(1,2) == 3
minhaSomaCurried = myCurry minhaSoma
-- minhaSomaCurried 1 2 == 3

-- UNCURRYING:
myUncurry :: (t -> u -> v) -> ((t,u) -> v)
myUncurry f (a,b) = f a b
-- minhaFuncao a b = a + b
-- Transforma isso em:
-- minhaFuncao(a,b) = a + b
-- EXEMPLO:
minhaSoma' a b = a + b
-- minhaSoma' 1 2 == 3
minhaSomaUncurried = myUncurry minhaSoma'
-- minhaSomaUncurried(1,2) == 3

-- FLIP: troca a ordem dos parâmetros
myFlip :: (t -> u -> v) -> (u -> t -> v)
myFlip f b a = f a b
subtracao = (-)
-- subtracao 5 2 == 3
subtracaoInvertida = myFlip subtracao
-- subtracaoInvertida 5 2 == -3