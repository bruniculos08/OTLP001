module Counter where

mdc' a b | a == b = a
        | a > b = mdc' (a - b) b
        | otherwise = mdc' a (b - a)

type Estado = Int


data MS a = ME (Estado -> (a, Estado))
-- O construtor ME recebe uma função que leva de um Int a um par (Int, a)

instance Functor MS where
    -- fmap :: (a -> b) -> MS a -> MS b
    fmap f (ME m) = ME (\e -> let (a, e') = m e in (f a, e'))
    -- Obs.: ote que m é uma função.

instance Applicative MS where
    -- pure :: a -> MS a
    pure x = ME (\e -> (x, e))
    ME fs <*> ME vs = ME (\e -> let (f', e') = fs e; (a, e'') = vs e' in (f' a, e''))

instance Monad MS where
    ME m >>= f = ME (\e -> let (a, e') = m e; ME fa = f a in fa e')

contador = ME (\x -> ((), x + 1))

teste = contador >>= \y -> return y
-- Isto é equivialente à:
-- ME (\e -> (x, e)) >>= \y -> return y
-- ME (\e -> let (a, e') = m e; ME fa = f a in fa e')

run (ME f) = f 0

mdc a b | a == b = do {contador; return a}
        | a > b = do {contador; mdc (a - b) b}
        | otherwise = do {contador; mdc a (b - a)}