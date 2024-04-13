data Semana = Dom | Seg | Ter | Qua | Qui | Sex | Sab deriving (Show, Eq, Ord)

-- Ao colocar deriving Show o compilador constrói automaticamente a função:
-- show :: Semana -> String
-- Ao colocar deriving Eq o compilador constrói automaticamente a função:
-- == :: Semana -> Semana -> Bool
-- Ao colocar deriving Ord o compilador constrói automaticamente a função:
-- > :: Semana -> Semana -> Bool

data Cadastro = Pessoa Int String String deriving Show

-- 'Pessoa' é um construtor de dados

prucurar cpf' [] = "" :: String
procurar cpf' (Pessoa cpf nome email : xs) = if cpf' == cpf then nome
                                            else procurar cpf' xs

-- Dado polimórfico (a e b são os tipos)

data Dupla a b = Par a b deriving Show

-- Lê-se, Dupla de tipo a e de tipo b
-- "Dupla" é um construtor de tipo (não de dados como é "Par") que recebe dois tipos (a e b) como parâmetro

fst (Par x y) = x
snd (Par x y) = y

data Lista a = Cons a (Lista a) | Nil deriving Show

-- Note que do lado direito da igualdade Cons recebe um elemento de tipo a enquanto Lista recebe um
-- tipo a (o tipo a), o que são coisas diferentes apesar de na escrita parecer a mesma, no entanto,
-- o compilador é capaz de notar essa diferança visto que Cons é um construtor de tipo dado enquanto
-- Lista é um construtor de tipo

tam Nil = 0
tam (Cons x xs) = 1 + tam xs

-- Obs.: aqui o tipo de retorna da função não é Int, é Num b pois a recursão usa a função "+"

data Arvore a = No a (Arvore a) (Arvore a) | Folha deriving Show

-- Sim dava pra fazer um tipo melhor de árvore em que as folhas tem elemento.

ins e Folha = No e Folha Folha
ins e a@(No x esq dir) | e == x = a
                       | e < x = No x (ins e esq) dir
                       | otherwise = No x esq (ins e dir)

-- Note que o "a" é um alias para o argumento (No x esq dir)
-- Note também que (como definido pela biblioteca) otherwise é equivalente a colocar True
-- Note que o tipo de "ins" é 
-- ins :: Ord a => a -> Arvore a -> Arvore a
-- Ord a significa que a é um elemento ordenável ("Ord a =>" é como "Para todo tipo a ordenável,")

-- Tarefa: fazer função que retorna uma lista ordenada dos elementos de uma árvore.

conc l1 Nil = l1
conc Nil l2 = l2
conc (Cons x xs) (Cons y ys) = Cons x (conc xs (Cons y ys))


imp Folha = Nil
imp (No x esq dir) = conc (imp esq) (conc (Cons x Nil) (imp dir))

-- Função de procurar para utilizar o tipo de dado maybe que é declarado na biblioteca da seguinte forma:]
-- data Maybe a = Just a | Nothing
-- Note que este tipo é útil para funções não totais 

procur e Folha = Nothing
procur e (No x esq dir) | e == x = Just e
                          | e < x = procur e esq
                          | otherwise = procur e dir

