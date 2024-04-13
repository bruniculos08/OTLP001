{-#LANGUAGE FunctionalDependencies#-}

data TermI = LitI Int | Succ TermI deriving Show
data TermB = LitB Bool | IsZero TermI deriving Show
data Term = If TermB Term Term | TB TermB | TI TermI deriving Show
data Res = RI Int | RB Bool

-- O tipo 'a' determina o tipo 'b'
class Eval a b | a -> b where
    eval :: a -> b

instance Eval TermI Int where
    eval (LitI i) = i
    eval (Succ t) = 1 + eval t

instance Eval TermB Bool where
    eval (LitB b) = b
    eval (IsZero t) = 0 == eval t

instance Eval Term Res where
    eval (if b )