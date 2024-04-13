{-#Language GADTSyntax#-}
-- Codificação de Huffman

data Bin = I Bin | O Bin | E

data Huffman where
    Node :: Int -> Huffman -> Huffman -> Huffman
    Leaf :: Int -> Char -> Huffman

instance Eq Huffman where
    (Node f1 tl1 tr1) == (Node f2 tl2 tr2) = (f1 == f2) && (tl1 == tl2) && (tr1 == tr2)
    (Leaf f1 c1) == (Leaf f2 c2) = (f1 == f2) && (c1 == c2)
    (Node _ _ _) == (Leaf _ _) = False
    (Leaf _ _) == (Node _ _ _) = False

instance Show Huffman where
    show (Node f tl tr) = "Node " ++ (show f) ++ " " ++ show tl ++ " " ++ show tr
    show (Leaf f c) = "Leaf " ++ (show f) ++ " " ++ (show c)

getFreq (Node f tl tr) = f
getFreq (Leaf f c) = f

getChar (Leaf f c) = c

getMinHuffman' [] a = a
getMinHuffman' (x : xs) a | getFreq x < getFreq a = getMinHuffman' xs a
                          | otherwise = getMinHuffman' xs a

getMinHuffman [] = (Leaf 0 ' ')
getMinHuffman (x : xs) = getMinHuffman' (x : xs) x

remHuffman [] _ = []
remHuffman (x : xs) a | x == a = xs
                      | otherwise = x : (remHuffman xs a)

remMinHuffman [] = []
remMinHuffman (x : xs) = remHuffman (x : xs) (getMinHuffman (x : xs))

-- Lembrar que este código vai transformar uma lista em uma lista de um elemento que será a árvore de Huffman:
huffman [] = Leaf 0 ' '
huffman [x] = x
huffman m@(x : xs)= huffman (Node (getFreq (getMinHuffman m) + getFreq (getMinHuffman (remMinHuffman m)))
                        (getMinHuffman m) (getMinHuffman (remMinHuffman m)) : remMinHuffman (remMinHuffman m))