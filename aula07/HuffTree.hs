{-#Language GADTSyntax#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module HuffTree where

import Data.List

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

instance Ord Huffman where
    Leaf f1 c1 <= Leaf f2 c2 = c1 <= c2
    Node f1 tl1 tr1 <= Node f2 tl2 tr2 = f1 <= f2
    Node f1 tl1 tr1 <= Leaf f2 c2 = False
    Leaf f1 c1 <= Node f2 tl2 tr2 = True

-- Esta função retira o valor f (frequência) de um nó ou folha da árvore de Huffman
getFreq :: Huffman -> Int
getFreq (Node f tl tr) = f
getFreq (Leaf f c) = f

-- Esta função retira o valor c (caracter) de uma folha da árvore de Huffman (nós não possuem caracter) 
getChar :: Huffman -> Char
getChar (Leaf f c) = c

-- Esta função retorna o nó ou folha com a menor frequência em uma árvore de Huffman considerando um nó ou folha a:
getMinHuffman' :: [Huffman] -> Huffman -> Huffman
getMinHuffman' [] a = a
getMinHuffman' (x : xs) a | getFreq x < getFreq a = getMinHuffman' xs x
                          | otherwise = getMinHuffman' xs a

-- Esta função retorna o nó ou folha com menor frequência de uma árvore de huffman (se a lista...
-- ... for vazia retorna um nó com frequência igual a 0):
getMinHuffman :: [Huffman] -> Huffman
getMinHuffman [] = Leaf 0 ' '
getMinHuffman (x : xs) = getMinHuffman' (x : xs) x

-- Esta função remove um nó ou folha de uma árvore de Huffman:
remHuffman :: [Huffman] -> Huffman -> [Huffman]
remHuffman [] _ = []
remHuffman (x : xs) a | x == a = xs
                      | otherwise = x : remHuffman xs a

-- Esta função remove o nó ou folha com a menor frequência de uma árvore de Huffman:
remMinHuffman :: [Huffman] -> [Huffman]
remMinHuffman [] = []
remMinHuffman (x : xs) = remHuffman (x : xs) (getMinHuffman (x : xs))

-- Esta função transforma uma lista de tipo Huffman em um elemento de Huffman que será a árvore de Huffman montada:
huffman :: [Huffman] -> Huffman
huffman [] = Leaf 0 ' '
huffman [x] = x
huffman m@(x : xs) = huffman (
                     let m1 = getMinHuffman m in let m2 = getMinHuffman (remMinHuffman m) in
                     let f1 = getFreq m1 in let f2 = getFreq m2 in
                     Node (f1 + f2) m1 m2 : remMinHuffman (remMinHuffman m))

-- Esta função recebe uma string e um caracter e retorna uma tupla com o caracter e sua frequência:
calcFreq :: Char -> String -> Int
calcFreq c "" = 0
calcFreq c (x : xs) | x == c = 1 + calcFreq c xs
                     | otherwise = calcFreq c xs

-- Esta função remove todos os caracteres iguais ao argumento 'e' de uma string:
remAll :: Char -> String -> String
remAll e "" = []
remAll e (x : xs) | e == x = remAll e xs
                   | otherwise = x : remAll e xs

-- Esta função é auxiliar da função seguinte (a lista retornada não é ordenado):
freqSimb' :: String -> [Huffman]
freqSimb' "" = []
freqSimb' (x : xs) = Leaf (calcFreq x (x : xs)) x : freqSimb' (remAll x xs)

-- Esta função transforma um string em uma lista de folhas de Huffman:
freqSimb :: String -> [Huffman]
freqSimb = sort . freqSimb'

-- Esta função é auxiliar da função seguinte (a lista retornada não é ordenado):
freqSimbExtra' :: [(Char, Int)] -> [Huffman]
freqSimbExtra' [] = []
freqSimbExtra' ((c, f) : t) = Leaf f c : freqSimbExtra t

-- Esta função transforma uma lista [(Char, Int)] em uma lista de folhas de Huffman:
freqSimbExtra :: [(Char, Int)] -> [Huffman]
freqSimbExtra = sort . freqSimbExtra'

-- Esta função transforme uma string em uma árvore de Huffman:
construirArvore :: String -> Huffman
construirArvore "" = Leaf 0 ' '
construirArvore str = huffman (freqSimb str)

-- Esta função adiciona um caracter à esquerda de todas as string de uma lista do tipo [(Maybe Char, String)]
addChar :: Char -> [(Char, String, Int)] -> [(Char, String, Int)] 
addChar c [] = []
addChar c ((x, y, f) : t) = (x, c : y, f) : addChar c t

-- Esta função é auxliar da função seguinte:
codHuffman' :: Huffman -> [(Char, String, Int)]
codHuffman' (Leaf f c) = [(c, "", f)]
codHuffman' (Node f l1 l2) = addChar '0' (codHuffman' l1) ++ addChar '1' (codHuffman' l2) 

-- Esta função gera o código para cada caracter da árvore de Huffman:
codHuffman :: Huffman -> [(Char, String, Int)]
codHuffman (Leaf f c) = [(c, "0", f)]
codHuffman (Node f l1 l2) = codHuffman' (Node f l1 l2)
-- Obs.: note que é necessário esta função superior para caso a árvore de Huffman tenha sido feita para apenas...
-- ... um caracter.

-- Esta função retorna o códico de um caracter numa lista que contém as seguintes informações [(caracter, código, frequência)]:
codChar :: Char -> [(Char, String, Int)] -> String
codChar c [] = ""
codChar c ((x, y, f) : t) | x == c = y
                          | otherwise = codChar c t
 
-- Esta função é auxiliar da função seguinte:
codificarAux :: String -> [(Char, String, Int)] -> String
codificarAux "" _ = ""
codificarAux (x : xs) t = codChar x t ++ codificarAux xs t
-- Obs.: esta função é necessária pois a próxima função usa a árvore de Huffman para criar uma...
-- ... lista com as seguintes informações [(caracter, código, frequência)], que esta usa para...
-- ... codificar a String.

-- Esta função codifica uma String numa sequência de 0's e 1's:
codificar :: String -> Huffman -> String
codificar "" _ = ""
codificar a@(x : xs) tree = codificarAux a (codHuffman tree) 

-- Esta função faz a codificação completa (não é necessário passar uma árvore Huffman como argumento)
codificarComp :: String -> String
codificarComp "" = ""
codificarComp a@(x : xs) = codificar a (construirArvore a)

-- Esta função recebe uma lista [(Char, String)] e retirar o caractar mais à esquerda de cada string:
remChar :: [(Char, String)] -> [(Char, String)]
remChar [] = []
remChar ((x, "") : t) = (x, "") : remChar t 
remChar ((x, y : ys) : t) = (x, ys) : remChar t 

-- Esta função é uma função auxiliar da função seguinte:
decodificarAux' :: String -> Huffman -> Maybe (Char, String)
decodificarAux' "" _ = Nothing
decodificarAux' _ (Leaf _ _ ) = Nothing
decodificarAux' ('0' : xs) (Node f (Leaf f1 c1) _) = Just (c1, xs)
decodificarAux' ('1' : xs) (Node f _ (Leaf f2 c2)) = Just (c2, xs)
decodificarAux' ('0' : xs) (Node f l1 l2) = decodificarAux xs l1
decodificarAux' ('1' : xs) (Node f l1 l2) = decodificarAux xs l2
decodificarAux' ( _ : xs) _ = Nothing
-- Obs.: note que esta função percorre a árvore e quando encontrar um caracter...
-- ... retorna um par com este caracter e com o resto (não consumido) da lista.

-- Esta função é uma função auxiliar da função seguinte:
decodificarAux :: String -> Huffman -> Maybe (Char, String)
decodificarAux "" _ = Nothing
decodificarAux _ (Leaf f c ) = Just (c, "0")
decodificarAux str (Node f l1 l2) = decodificarAux' str (Node f l1 l2)
-- Obs.: esta função superior à função decodificarAux' é necessária devido a possibilidade...
-- ... da árvore de Huffman ter sido feita para apenas um caracter.

-- Esta função decodifica uma String binária em uma String (ascii-8):
decodificar :: String -> Huffman -> String
decodificar "" _ = ""
decodificar a@(x : xs) tree = let Just (c, rest) = decodificarAux a tree in c : decodificar rest tree

-- Esta função codifica um texto e devolve um tupla com o texto codificado e a árvore de Huffman:
codificarTexto :: String -> (String, Huffman, [(Char, String, Int)])
codificarTexto "" = ("", Leaf 0 ' ', [])
codificarTexto a@(x : xs) = let tree = construirArvore a in (codificar a tree, tree, codHuffman tree)
-- Obs.: a tupla contém as seguintes informações (texto, árvore de Huffman, [(caracter, código, frequência)]).