
module ExemploSemMonda where

data Term = Const Int | Add Term Term | Sub Term Term | Mul Term Term | Div Term Term deriving Show

eval (Add t1 t2) = case eval t1 of 
                    Just v1 -> case eval t2 of 
                        Just v2 -> Just (v1 + v2)
                        Nothing -> Nothing
                    Nothing -> Nothing

eval (Sub t1 t2) = case eval t1 of 
                    Just v1 -> case eval t2 of 
                        Just v2 -> Just (v1 - v2)
                        Nothing -> Nothing
                    Nothing -> Nothing

eval (Mul t1 t2) = case eval t1 of 
                    Just v1 -> case eval t2 of 
                        Just v2 -> Just (v1 + v2)
                        Nothing -> Nothing
                    Nothing -> Nothing

eval (Div t1 t2) = case eval t1 of 
                    Just v1 -> case eval t2 of 
                        Just v2 -> if v2 == 0 then Nothing else Just (div v1 v2)
                        Nothing -> Nothing
                    Nothing -> Nothing

eval (Const i) = Just i     