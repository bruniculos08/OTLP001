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