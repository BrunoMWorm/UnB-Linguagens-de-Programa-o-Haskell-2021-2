-- Definindo os tipos para nosso banco de dados
type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase = [("Alice","Postman Pat"),
            ("Anna","All Alone"),
            ("Alice","Spot"),
            ("Rory","Postman Pat")]

-- Consultas sobre o banco de dados
books :: Database -> Person -> [Book]
borrowers :: Database -> Book -> [Person]
borrowed :: Database -> Book -> Bool
numBorrowed :: Database -> Person -> Int

-- Atualizações no banco de dados
makeLoan :: Database -> Person -> Book -> Database
returnLoan :: Database -> Person -> Book -> Database

-- Implementações: sem usar List Comprehension
books [] _ = []
books ((p,b):xs) person
    | person == p = b : books xs p
    | otherwise = books xs p

borrowers [] _ = []
borrowers ((p,b):xs) book
    | book == b = p : borrowers xs b
    | otherwise = borrowers xs b

borrowed [] _ = False
borrowed ((p,b):xs) book
    | book == b = True
    | otherwise = borrowed xs b

numBorrowed [] _ = 0
numBorrowed ((p,b):xs) person
    | person == p = 1 + numBorrowed xs person
    | otherwise = numBorrowed xs person

makeLoan db person book = (person,book) : db

returnLoan [] _ _ = []
returnLoan ((p,b):xs) person book
    | (person == p) && (book == b) = xs
    | otherwise = (p,b) : returnLoan xs person book



-- Implementações com LC
booksLC db person = [(p,b) | (p,b) <- db, person == p]
borrowersLC db book = [(p,b) | (p,b) <- db, book == b]
borrowedLC db book = not $ null [(p,b) | (p,b) <- db, book == b]
numBorrowedLC db person = length $ [(p,b) | (p,b) <- db, person == p]



-- Faz sentido para o makeLoan e para o returnLoan?
-- A implementação abaixo está agindo como um filtro, removendo TODOS registros de empréstimos.
returnLoanLC db person book = [(p,b) |(p,b) <- db, (p /= person) && (b /= book)]