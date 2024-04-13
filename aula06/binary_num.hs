{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import Data.Word
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
-- A seguinte biblioteca serve para operações com bits que são .&., .|., xor, complement, shift
import Data.Bits

type Reg = (Word8, Word32) 

freq [] = []
freq (x : xs) = (x, length l1 + 1) : freq l2 
                where (l1, l2) = partition (==x) xs

-- partition divide a lista em uma lista que satisfaz o predicado e outra que não satisfaz (retornando um 2-upla).

-- Uma lista de bytes se propaga na seguinte monada:
put [] = P.flush
put ((c, f) : xs) = 
                    do
                    P.putWord8 (I.c2w c)
                    P.putWord32be (toEnum f) -- Big Endian
                    put xs

-- A monada propaga um dado (isso é definido na própria monada), por isso as funções no bloco devem...
-- ... possuir o mesmo tipo).

escrita =
        do
        txt <- readFile "Exemplo.txt"
        let xs = freq txt
        let bs = P.runPut (put xs)
        putStr (show xs)
        L.writeFile "teste.bin" bs

leitura = 
        do
        bs <- L.readFile "teste.bin"
        let rs = G.runGet getRegs bs
        printRegs rs

getReg = 
        do
        c <- G.getWord8
        f <- G.getWord32be
        return (c, f)

-- A seguinte função é utilizada na função leitura; note que ela não precisa receber o argumento 'bs' pois esse...
-- ... argumento está sendo repassado na mônada em que ele é executado (por propagação de estado) 
getRegs =
        do
        empty <- G.isEmpty
        if empty then return []
        else do {r <- getReg; rs <- getRegs; return (r : rs)}

printRegs [] = return ()
print ((c, f) : rs) = 
                        do 
                        putStrLn ((show (I.w2c c)) ++ "-" ++ show f ++ "\n")
                        printRegs rs


-- função que converte string com 8 caracteres para Word8
s2B :: String -> Word8
s2B "" = 0
s2B (x : xs) | x == '0' = s2B xs
             | x == '1' = shiftL 1 (length xs) + s2B xs


 