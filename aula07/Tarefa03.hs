-- Aluno: Bruno Rafael dos Santos
{-#Language GADTSyntax#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List
import Data.Word
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import System.FilePath.Posix
-- A seguinte biblioteca serve para operações com bits que são .&., .|., xor, complement, shift
import Data.Bits
import qualified Data.Text as DT
import qualified Data.String as DS

-------------------------------- Codificação de Huffman --------------------------------

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


----------------------- Codificação em bytes e leitura e escrita de arquivos -----------------------

type Reg = (Word8, Word32)

-- Esta função converte string com 8 caracteres para Word8:
str2Byte :: String -> Word8
str2Byte "" = 0
str2Byte (x : xs) | x == '0' = str2Byte xs
                  | x == '1' = shiftL 1 (length xs) + str2Byte xs

-- Esta função é auxiliar a função seguinte
byte2Str' :: Word8 -> Int -> String
byte2Str' _ 0 = ""
byte2Str' byte n | shiftL 1 (n - 1) .&. byte == fromInteger (2 ^ (n - 1)) = '1' : byte2Str' byte (n - 1)
                 | otherwise = '0' : byte2Str' byte (n - 1)

-- Esta função converte Word8 para uma string de 0's e 1's:
byte2Str :: Word8 -> String
byte2Str byte = byte2Str' byte 8

-- Esta função converte [Word8] para uma string de 0's e 1's:
byteList2Str :: [Word8] -> String
-- byteList2Str [] = []
-- byteList2Str (b : bs) = byte2Str b ++ byteList2Str bs
byteList2Str = concatMap byte2Str

-- Esta função recebe uma String e um Int n e retorn uma substring de tamanho n ou a própria String de argumento...
-- ... caso seu tamanho seja menor que n:
subStr :: String -> Int -> String
subStr "" _ = ""
subStr _ 0 = ""
subStr (x : xs) n = x : subStr xs (n - 1)

-- Esta função recebe uma String e um Int n e retorna a String com os n primeiros caracteres removidos:
remFrontStr :: String -> Int -> String
remFrontStr "" _ = ""
remFrontStr str 0 = str
remFrontStr (x : xs) n = remFrontStr xs (n - 1)

-- Esta função recebe um Char e um Int n e retorna uma String com o Char repetido n vezes:
repeatChar :: Char -> Int -> String
repeatChar _ 0 = ""
repeatChar c n = c : repeatChar c (n - 1)

-- Esta função converte uma String de 0's e 1's em uma String de tamanho divisível por 8 (preenche com 0's o final...
-- até que o número seja divisível por 0)
divEightStr :: String -> String
divEightStr "" = ""
divEightStr str@(x : xs) | length (subStr str 8) == 8 = subStr str 8 ++ divEightStr (remFrontStr str 8)
                         | otherwise = let rest = subStr str 8 in rest ++ repeatChar '0' (8 - length rest)

-- Esta função é uma função auxiliar da função seguinte:
texto2Bytes' :: String -> [Word8]
texto2Bytes' "" = []
texto2Bytes' str@(x : xs) = str2Byte (subStr str 8) : texto2Bytes' (remFrontStr str 8)

-- Esta função converte uma string de 0's e 1's em um lista de Word8:
texto2Bytes :: String -> [Word8]
texto2Bytes str = texto2Bytes' (divEightStr str)

-- Esta função conta o número de símbolos de uma árvore de Huffman:
countSimbTree :: Huffman -> Word8
countSimbTree (Leaf _ _) = 1
countSimbTree (Node f l1 l2) = countSimbTree l1 + countSimbTree l2
-- Obs.: para otmização algo pode ser feito para que tal contagem armazenada no momento...
-- ... em que a árvore é montada (este cálculo é feito internamente na função freqSimb pela...
-- ... função auxiliar calFreq).

-- Esta função codifica um texto, seguindo o algoritmo de Huffman, como [Word8]:
codificarTexto2Bin :: String -> (Word8, Word32, [Word8], [(Char, String, Int)])
codificarTexto2Bin str = let (text, tree, code) = codificarTexto str in
                        (countSimbTree tree, fromIntegral (getFreq tree), texto2Bytes text, code)
-- Obs.: esta função retorna o desto compactado exatamente da maneira descrita no PDF dos exercicíos.

------- Nesta parte do arquivo tem-se a funções necessárias para leitura e escrita do arquivo -------

-- Esta função calcula a frequência de cada elemento em uma lista:
freq [] = []
freq (x : xs) = (x, length l1 + 1) : freq l2
                where (l1, l2) = partition (==x) xs
-- Obs.: partition divide a lista em uma lista que satisfaz o predicado e outra que não satisfaz (retornando um 2-upla).

-- Esta função aloca no buffer uma lista lista de Word8 (byte), cada um, seguidos por um Word32:
put [] = P.flush
put ((c, f) : xs) =
                    do
                    P.putWord8 (I.c2w c)
                    P.putWord32be (toEnum f) -- be significa "Big Endian".
                    put xs

-- Uma lista de Word8 (byte), cada um, seguidos por um Word32, se propaga na seguinte monada...
-- ... (isto é, são colocados no outputBuffer).
-- A monada propaga um dado (isso é definido na própria monada), por isso as funções no bloco devem...
-- ... possuir o mesmo tipo).

-- Esta função aloca no buffer uma lista de Word8 (é igual a função acima, mas a lista alocata é só de Word8):
putOnlyBytes [] = P.flush
putOnlyBytes (x : xs) =
                    do
                    P.putWord8 x
                    putOnlyBytes xs

-- Esta função lê um texto (src), faz uma lista de (caracter, frequência) e converte essa lista de uma lista...
-- ... [(Char, Int)] para uma lista [(Word8, Word32)] e então escreve isso em um arquivo binário (dest):
escritaExemplo src dest =
                do
                txt <- readFile src
                let xs = freq txt
                let bs = P.runPut (put xs)
                putStr (show xs ++ "\n")
                L.writeFile dest bs

-- Esta função é auxiliar da função seguinte:
converterListaDeFreq :: [(Char, String, Int)] -> [(Char, Int)]
converterListaDeFreq [] = []
converterListaDeFreq ((c, code, f) : xs) = (c, f) : converterListaDeFreq xs

-- Esta função recebe um nome para arquivo e as informaçãos sobre um texto codificado e escreve em...
-- ... um arquivo a tabela exposta na lista de execícios:
escritaCompactada :: FilePath -> (Word8, Word32, [Word8], [(Char, String, Int)]) -> IO ()
escritaCompactada dest info@(num_char, total_char, texto_codificado, sub_info) =
                                    do
                                    if total_char == 0 then L.writeFile dest L.empty
                                    else
                                        do
                                        let nt = P.runPut (put [(I.w2c num_char, fromIntegral total_char)])
                                        let char_freq = P.runPut (put (converterListaDeFreq sub_info))
                                        let texto_codificado' = P.runPut (putOnlyBytes texto_codificado)
                                        L.writeFile dest (nt <> char_freq <> texto_codificado')

-- Esta função lê o arquivo src e escreve em um arquivo dest a tabela de compactação proposta...
-- ... na lista de exercícios: 
gerarArqCompactado :: FilePath -> IO()
gerarArqCompactado src = do
                         let dest = takeBaseName src ++ " - compactado"
                         txt <- readFile src
                         let info = codificarTexto2Bin txt
                         escritaCompactada dest info

-- A seguinte função retira de um buffer que é uma lista de dados dois dados, convertendo-os para Word8...
-- ... e Word32be respectivamente, e retorna um par com estes 2 dados retirados:
getReg =
        do
        c <- G.getWord8
        f <- G.getWord32be
        return (c, f)

-- A seguinte função é utilizada na função leitura; note que ela não precisa receber o argumento 'bs' pois esse...
-- ... argumento está sendo repassado na mônada em que ele é executado (por propagação de estado):
getRegs =
        do
        empty <- G.isEmpty
        if empty then return []
        else do {r <- getReg; rs <- getRegs; return (r : rs)}

-- A seguinte função imprime a lista de tipo [(Word8, Word32be)] convertando para forma [(Char, Int)]:
printRegs [] = return ()
printRegs ((c, f) : rs) =
                        do
                        putStrLn ((show (I.w2c c)) ++ "-" ++ show f ++ "\n")
                        printRegs rs

-- Esta função lê um arquivo binário contendo uma lista [(Word8, Word32)] e imprime essa lista como [(Char, Int)]: 
leituraExemplo src =
                    do
                    bs <- L.readFile src
                    let rs = G.runGet getRegs bs
                    printRegs rs

-- Esta função lê uma ByteString e retorna [Word8]:
getBytes =
            do
            empty <- G.isEmpty
            if empty then return []
            else do {r <- G.getWord8; rs <- getBytes; return (r : rs)}

-- Esta função lê uma ByteString com n bytes e retorna [Word8] (de tamanho 8):
getNBytes n =
                do
                empty <- G.isEmpty
                if n == 0 || empty then return []
                else do {r <- G.getWord8; rs <- getNBytes (n - 1); return (r : rs)}

-- Esta função lê uma ByteString com (n * (8 + 32)) = 40 * n bytes e retorna [(Word8, Word32)]:
getNPairs n =
                do
                if n == 0 then return []
                else do {c <- G.getWord8; f <- G.getWord32be; t <- getNPairs (n - 1); return ((c, f) : t)}

-- Lê o número de caracteres diferentes e o número total de caracteres do cabeçalho:
leituraHeader =
                do
                n <- G.getWord8
                t <- G.getWord32be
                return (n, t)

-- Converte uma lista com informação [(caracter, frequencia)] de [(Word8, Word32)] para [(Char, Int)]:
converterParesCharFreq :: [(Word8, Word32)] -> [(Char, Int)]
converterParesCharFreq [] = []
converterParesCharFreq ((x, y) : t) = (I.w2c x, fromIntegral y) : converterParesCharFreq t

-- Esta função decodifica uma String binária em uma String:
decodificarN :: Int-> String -> Huffman -> String
decodificarN _ "" _ = ""
decodificarN 0 _ _ = ""
decodificarN n a@(x : xs) tree = let Just (c, rest) = decodificarAux a tree in c : decodificarN (n-1) rest tree

--
divList 0 a = ([], a)
divList _ [] = ([], [])
divList n a@(x : xs) = let (l1, l2) = divList (n - 1) xs in (x : l1, l2)

-- Esta função converte 4 Word8 em um Word32:
converterW82W32 :: [Word8] -> Word32
converterW82W32 = foldl' acumulador 0 where
                  acumulador w32 w8 = shiftL w32 8 .|. fromIntegral w8

-- Esta função obtém o cabeçalho separado na lista de pares (caracter, frequência) e na lista de caracteres (tudo em Word8 e Word32):
getHeader :: Int -> [Word8] -> ([(Word8, Word32)], [Word8])
getHeader 0 a = ([], a)
getHeader _ [] = ([], [])
getHeader n a@(w8 : t) = let (w32, rest) = divList 4 t in
                let (a1, a2) = getHeader (n - 1) rest in
                ((w8, converterW82W32 w32) : a1, a2)


-- Esta função lê o arquivo compactado e transforma em uma tupla com as informações, do... 
-- ... tipo (Word8, Word32, [Word8], [(Char, String, Int)]) para remontar o texto original:
leituraArqCompactado src =  do
                            let dest = DT.unpack (DT.replace (DS.fromString " - compactado") (DS.fromString "") (DS.fromString (takeBaseName src))) ++ " - descompactado.txt"
                            bytes <- L.readFile src
                            if bytes == L.empty then writeFile (DS.fromString dest) ""
                            else 
                                do
                                let (n, t) = G.runGet leituraHeader bytes
                                let (_, to_format_bytes) = divList 5 (G.runGet getBytes bytes)
                                let (pairs, texto_codificado) = getHeader (fromIntegral n) to_format_bytes
                                let tree = huffman (freqSimbExtra (converterParesCharFreq pairs))
                                let texto_decodificado = decodificarN (fromIntegral t) (byteList2Str texto_codificado) tree
                                putStrLn texto_decodificado
                                writeFile (DS.fromString dest) texto_decodificado