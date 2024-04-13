import Data.List

replicateE x 0 = []
replicateE x n = x : (replicateE x  (n - 1))

data Matrix a = Matrix [[a]] deriving Eq

size [] = 0
size (x : xs) = 1 + size xs

linesNum (Matrix []) = 0
linesNum (Matrix (l : ls)) = size (l : ls)

colsNum (Matrix []) = 0
colsNum (Matrix (l:ls)) = size l

sizeMatrix (Matrix []) = (0, 0)
sizeMatrix m = (linesNum m, colsNum m)

-- Função que gera matriz m x n:

createMatrix m n = Matrix (replicateE (replicateE 0 n) m)

-- Função que insere numa posição (i,j) da matriz:

insertList (x : xs) e 0 = e : xs
insertList (x : xs) e p = x : (insertList xs e (p - 1))

insertMatrix' m@(Matrix (l:ls)) e 0 j = (insertList l e j) : ls
insertMatrix' m@(Matrix (l:ls)) e i j = l : (insertMatrix' (Matrix ls) e (i - 1) j)

insertMatrix m e i j = if (i < (linesNum m)) && (j < (colsNum m)) then Matrix (insertMatrix' m e i j)
                      else m

-- Gerador de matriz por função f(i,j):

mapLine' [] j _ = []
mapLine' (x : xs) j f = f j : (mapLine' xs (j + 1) f)

mapLine [] _ = []
mapLine (x : xs) f = mapLine' (x : xs) 0 f

mapMatrix' m n _ _ [] _ = []
mapMatrix' m n i j (l : ls) f = (mapLine l (f i)) : (mapMatrix' m n (i + 1) j ls f)

mapMatrix (Matrix []) _ = Matrix []
mapMatrix m@(Matrix (l:ls)) f = Matrix (mapMatrix' ((linesNum m) - 1) ((colsNum m) - 1) 0 0 (l:ls) f)
                    
instance Show a => Show (Matrix a)
  where
    show (Matrix a) = intercalate "\n" $ map (intercalate " " . map show) a