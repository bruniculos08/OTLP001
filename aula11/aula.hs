fix f = let x = f x in x 
fat n = fix (\f n -> if n == 0 then 1 else n * f (n - 1)) n