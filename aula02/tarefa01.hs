-- Aluno: Bruno Rafael dos Santos
data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

ins e Leaf = Node e Leaf Leaf
ins e n@(Node x l r)
 | e == x = n
 | e < x = Node x (ins e l) r
 | otherwise = Node x l (ins e r)

-- Lembrar que não é possível comparar uma variável com um construtor de árvore pois a operação "==" não está definida...
-- ... para o tipo "Tree"

-- Remoção:
greatest :: (Ord a) => Tree a -> Maybe a
greatest Leaf = Nothing
greatest (Node x y Leaf) = Just x
greatest (Node x y z) = greatest z

rem_greatest :: (Ord a) => Tree a -> Tree a
rem_greatest Leaf = Leaf
rem_greatest (Node x y Leaf) = y
rem_greatest (Node x y z)= Node x y (rem_greatest z)

remove e Leaf = Leaf
remove e (Node x xl xr) | e == x = if (greatest xl) == Nothing then xr
                     else Node (maybe e id (greatest xl)) (rem_greatest xl) xr
                     | e < x = Node x (remove e xl) xr
                     | otherwise = Node x xl (remove e xr)


-- Imprimir árvore
printBT' Leaf level = []
printBT' (Node x Leaf Leaf) level = [(x, level)]
printBT' (Node x (Node y yl yr) Leaf) level = [(x, level), (y, level + 1)] ++ (printBT' yl (level + 2)) ++ (printBT' yr (level + 2)) 
printBT' (Node x Leaf (Node y yl yr)) level = [(x, level), (y, level + 1)] ++ (printBT' yl (level + 2)) ++ (printBT' yr (level + 2)) 
printBT' (Node x (Node y yl yr) (Node z zl zr)) level = [(x, level), (y, level + 1), (z, level + 1)] ++ 
    (printBT' yl (level + 2)) ++ (printBT' yr (level + 2)) ++ (printBT' zl (level + 2)) ++ (printBT' zr (level + 2)) 

printBT tree = printBT' tree 0

-- Árvore Rubro-Negra

data Color = R | B deriving Show

data RB a = No a Color (RB a) (RB a) | Folha

-- Rotações:
rot Folha = Folha
-- Caso: nó preto com um filho a direita vermelho que tem também um filho a direita vermelho
rot (No x B a (No y R b (No z R c d))) = No y R (No x B a b) (No z B c d)
-- Caso: nó preto com um filho a esquerda vermelho que tem também um filho a esquerda vermelho
rot (No x B (No y R (No z R c d) b) a) = No y R (No z B c d) (No x B b a)
-- Caso: nó preto com um filho a direita vermelho que tem um filho a esquerda vermelho
rot (No x B a (No y R (No z R c d) b)) = No z R (No x B a c) (No y B d b)
-- Caso: nó preto com um filho a esquerda vermelho que tem um filho a direita vermelho
rot (No x B (No y R b (No z R c d)) a) = No z R (No y B b c) (No x B d a)
-- Casos que estão de acordo com as regras da árvore rubro-negra:
rot tree = tree

-- Colorir a raiz de preto sempre:
paint Folha = Folha
paint (No x _ a b) = No x B a b

-- -- Balanceamento (Obs.: posições em relação ao avô):
-- bal Folha = Folha
-- -- Caso: avô de qualquer cor, pai de cor vermelha a direita, filho de cor vermelha a direita e tio vermelho a esquerda
-- bal (No grandfather _ (No uncle R a b) (No father R c (No son R d e))) = No grandfather R (No uncle B a b) (No father B c (No son R d e))
-- -- Caso: avô de qualquer cor, pai de cor vermelha a direita, filho de cor vermelha a esquerda e tio vermelho a esquerda
-- bal (No grandfather _ (No uncle R a b) (No father R (No son R d e) c)) = No grandfather R (No uncle B a b) (No father B (No son R d e) c)
-- -- Caso: avô de qualquer cor, pai de cor vermelha a esquerda, filho de cor vermelha a esquerda e tio vermelho a direita

-- Inserção:
ins' e Folha = No e R Folha Folha
ins' e (No x cor esq dir) | e >= x = rot (No x cor esq (ins' e dir))
                          | otherwise = rot (No x cor (ins' e esq) dir)

insRB e tree = paint (ins' e tree)  

-- Imprimir árvore RB:
printRB' Folha _ = []
-- Caso: nó com dois filhos
printRB' (No n0 c0 (No n1 c1 a b) (No n2 c2 c d)) level = [(n0, c0, level), (n1, c1, level + 1), (n2, c2, level + 1)] ++ 
    printRB' a (level + 2) ++ printRB' b (level + 2) ++ printRB' c (level + 2) ++ printRB' d (level + 2)
-- Caso: nó com filho a direita apenas
printRB' (No n0 c0 Folha (No n2 c2 c d)) level = [(n0, c0, level), (n2, c2, level + 1)] ++ 
    printRB' c (level + 2) ++ printRB' d (level + 2)
-- Caso: nó com filho a esquerda apenas
printRB' (No n0 c0 (No n1 c1 a b) Folha) level = [(n0, c0, level), (n1, c1, level + 1)] ++ 
    printRB' a (level + 2) ++ printRB' b (level + 2)
-- Caso: nó sem filhos
printRB' (No n0 c0 Folha Folha) level = [(n0, c0, level)]

printRB tree = printRB' tree 0 

simple_print Folha = []
simple_print (No x cor esq dir) = [x] ++ simple_print esq ++ simple_print dir
