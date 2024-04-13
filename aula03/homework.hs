-- Aluno: Bruno Rafael dos Santos
{-# LANGUAGE GADTSyntax#-}

data Bin = V | Z Bin | U Bin
data SigBin = Neg Bin | Pos Bin

instance Show Bin where
    show V = ""
    show (Z n) = "0" ++ show n
    show (U n) = "1" ++ show n

instance Show SigBin where
    show (Pos V) = ""
    show (Neg V) = ""
    show (Pos n) = " + " ++ show n
    show (Neg n) = " - " ++ show n

sizeBin V = 0
sizeBin (Z n) = 1 + sizeBin n
sizeBin (U n) = 1 + sizeBin n

binToInteger V = 0
binToInteger (Z n) = 0 + binToInteger n
binToInteger (U n) = (2 ^ (sizeBin n)) + (binToInteger n)

sigBinToInteger (Neg n) = - (binToInteger n)
sigBinToInteger (Pos n) = binToInteger n

getCarry V = True
getCarry (U V) = True 
getCarry (Z n) = False  
getCarry (U n) = getCarry n 

invertBin V = V
invertBin (Z n) = U (invertBin n)
invertBin (U n) = Z (invertBin n)

incrBin V = V
incrBin (Z n) | getCarry n == True = U (invertBin n)
              | otherwise = Z (incrBin n)
incrBin (U n) | getCarry n == True = U (Z (invertBin n))
              | otherwise = U (incrBin n)

natToBin 0 = Z V
natToBin n = incrBin (natToBin (n - 1))

integerToSigBin 0 = Pos (Z V)
integerToSigBin n | n < 0 = Neg (natToBin (abs n))
                  | otherwise = Pos (natToBin n)

instance Num SigBin where
    -- (+) :: SigBin -> SigBin -> SigBin
    a + b = integerToSigBin (sigBinToInteger a + sigBinToInteger b)

    -- (-) :: SigBin -> SigBin -> SigBin
    a - b = integerToSigBin (sigBinToInteger a - sigBinToInteger b)

    -- (*) :: SigBin -> SigBin -> SigBin
    a * b = integerToSigBin (sigBinToInteger a * sigBinToInteger b)

    -- abs :: SigBin -> SigBin
    abs (Pos n) = Pos n
    abs (Neg n) = Pos n

    -- signum :: SigBin -> SigBin
    signum (Pos V) = Pos V
    signum (Neg V) = Pos V
    signum (Pos (Z V)) = Pos (Z V)
    signum (Neg (Z V)) = Pos (Z V)
    signum (Pos (U n)) = Pos (U V) 
    signum (Neg (U n)) = Neg (U V) 
    signum (Pos (Z n)) = signum (Pos n) 
    signum (Neg (Z n)) = signum (Neg n) 

    -- fromInteger :: Integer -> SigBin
    fromInteger = integerToSigBin