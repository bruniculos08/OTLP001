module ExemploComMonda where

data Term = Const Int | Add Term Term | Sub Term Term | Mul Term Term | Div Term Term deriving Show

eval (Const i) = return i

eval (Add t1 t2) = do {v1 <- eval t1; v2 <- eval t2; return (v1 + v2)}

eval (Sub t1 t2) = do {v1 <- eval t1; v2 <- eval t2; return (v1 - v2)}
-- eval (Sub t1 t2) = eval t1 >>= \v1 -> (eval t2 >>= \v2 -> return (v1 - v2))

eval (Mul t1 t2) = do {v1 <- eval t1; v2 <- eval t2; return (v1 * v2)}

eval (Div t1 t2) = do {v1 <- eval t1; v2 <- eval t2; if v2 == 0 then Nothing else return (div v1 v2)}

-- Estamos usando aqui o tipo Maybe com mônada

-- Declaração da mônada com Maybe na biblioteca

data Maybe' a = Just' a | Nothing'

-- instance Monad Maybe' where
    -- return x = Just' x
    -- O operador recebe o dado Nothing' e uma função e retorna Nothing' direto (não usa a função f pra nada)
    -- Nothing' >>= f = Nothing'
    -- O operador recebe o dado Just' x e uma função e aplica a função sobre esse dado
    -- Just' x >>= f = f x

-- Uma operação binária associativa com elemento neutro é um monoíde. Uma mônada difere...
-- ... de um monoíde por o operando a direita envolver uma vinculação (bind).
--                                
--              return a >>= λb.n = n[a/b]
--              m >>= λa.return a = m
--              m >>= (λa.n >>= λb.o) = (m >>= λa.n) >>= λb.o

data Log a = ML (String, a) deriving Show

instance Functor Log where
    fmap f (ML (s, x)) = ML (s, f x)

instance Applicative Log where
    -- (<*>) :: Log (a -> b) -> Log a -> Log b
    ML (s1, f) <*> ML (s2, x) = ML (s1 ++ s2, f x)  
    -- o pure é equivalente ao return
    pure x = ML ("", x)

instance Monad Log where
    -- ML m >>= f = let (s, a) = m in 
    --                 let ML(s', b) = f a in
    --                     ML (s ++ s', b)
    
    ML (s, a) >>= f = let ML (s', b) = f a
                            in ML (s ++ s', b)


tlog s = ML (s ++ "\n", ())

teste1 = do 
            tlog "teste1"
            tlog "teste2"
            return ()

teste2 = do 
            tlog "teste1"
            tlog "teste2"
            return "teste3"

teste3 = do 
            tlog "teste1"
            tlog "teste2"
            pure "teste3"

teste4 = do 
            tlog "teste1"
            tlog "teste2"
            pure "teste3"
            return ()
