-- Mônada IO

-- return : 
-- (>>) : chama a próxima função
-- (>>=) : passa o argumento da função a esquerda para a função seguinte

-- return :: a -> M a 
-- (>>) :: M a -> M b -> M b
-- (>>=) :: Ma -> a -> M b -> M b 

main = putStr  "Qual seu nome? " >> getLine >>= (\nome -> putStr ("Alo " ++ nome ++ "\n"))

main_01 = 
        do 
        putStr "Qual seu nome?"
        nome <- getLine
        putStr ("Alo " ++ nome ++ "\n")

fat 0 = 1
fat n = n * fat(n-1)

main_02 =
        do
        putStr "Digite um número: "
        num <- getLine
        -- n <- fat (read num)          <= Assim não funciona por que o tipo de mnada das funções anteriores é IO()
        let n = fat (read num)
        putStr ("Resultado: " ++ show n)

-- A função acima é equivalente ao seguinte:
-- putStr "Digite um número." >> getLine >>= (\num -> let n = fat(num) in PutStr "Resultado = " ++ (show num))
-- Note que a última instrução deve possuir tipo IO()

-- Leitura de arquivo em Haskell:
main_file =
            do
            putStr "Nome: "
            name <- getLine
            txt <- readFile name
            let t = length txt
            putStr (show t ++ "caracteres\n")

-- Lembrar: haskell têm avaliação preguiçosa (é como se os valores da variável t não fosse calculado e sim fosse anotada...
-- ... a expressão que resulta em seu valor), assim o valor de t só é calculado na chamada 'show t'.

-- em dois let's seguidos para mesma variável o que vale é o resultado da seguinte pois a espressão mônada resultante é...
-- ... equivalente a: let x = 1 in let x = 2 in
-- o resultado do segundo let esconde o que foi feito pelo primeiro, é como se fosse um x local vs um x global e C/C++.