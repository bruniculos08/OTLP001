-- Arquivo para treinar

re x [] = []
re x (y : ys) | y == x = ys
            | otherwise = (y : (re x ys))

get_min [] = -1
get_min [x] = x
get_min (x : (y : xs)) | x < y = get_min (x : xs)  
                     | otherwise = get_min (y : xs)
                     
simple_sort [] = []
simple_sort (x : xs) | x == (get_min (x : xs)) = (x : (simple_sort xs))
                      | otherwise = ((get_min xs) : (simple_sort(re (get_min xs) (x : xs))))

fst' (a, b, c) = a
snd' (a, b, c) = b
thr' (a, b, c) = c

my_mod :: Int -> Int -> Int
my_mod 0 _ = 0
my_mod a 0 = a
my_mod a b | (a < 0) && (b < 0) = my_mod (-1 * a) (-1 * b)
        | (a < 0) = my_mod (a + b) b
        | (b < 0) = my_mod (a + b) b
        | b <= a = my_mod (a - b) b
        | otherwise = a

-- a * x + b * y = gcd(a, b)
-- retorno (gcd, x, y)
extended_euclides :: Integer -> Integer -> (Integer, Integer, Integer)
extended_euclides a 0 = ((abs a), 1, 0)
extended_euclides a b = (fst' (extended_euclides b (mod a b)), thr' (extended_euclides b (mod a b)), 
    (snd' (extended_euclides b (mod a b))) - (div a b) * (thr' (extended_euclides b (mod a b))) )

-- Program Fixpoint Euclides_Ext (a b : Z) 
--     {measure (Z.abs_nat (Z.abs b))} :=
--     match b with
--         | 0 => (a, 1, 0)
--         | _ => let '(g, x', y') := Euclides_Ext b (a mod b)
--         in (g, y', x' - (a/b) * y')
--     end.    