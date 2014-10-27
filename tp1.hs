-- Ejercicio 1 :

long :: [a] -> Integer
long [] = 0
long (x:xs) = 1 + long xs


-- Ejercicio 2 :

e :: Integer -> Integer -> [Integer]
e n k = concat (e' (eAux k 1) (eAux (n - k) 0))

eAux :: Integer -> Integer -> [[Integer]]
eAux 0 _ = []
eAux n m = [m] : (eAux (n - 1) m)
 
e' :: [[a]] -> [[a]] -> [[a]]
e' xs [] = xs
e' xs ys | long xs >= long ys = e' (agrupar (mezclar (take' lys xs) ys)) (drop' lys xs)
         |          otherwise = e' (agrupar (mezclar xs (take' lxs ys))) (drop' lxs ys)
         where long ys = lys
         where long xs = lxs

take' :: Integer -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : (take' (n-1) xs) 

drop' :: Integer -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

mezclar :: [a] -> [a] -> [a]
mezclar [] ls = ls
mezclar xs [] = xs
mezclar (x:xs) (l:ls) = [x,l] ++ (mezclar xs ls)

agrupar :: [[a]] -> [[a]]
agrupar [] = []
agrupar (x:y:xs) = [x++y]++(agrupar xs)


-- Ejercicio 3

equivalentes :: [Integer] -> [Integer] -> Bool
equivalentes xs ys = long xs == long ys && equivalentes' xs ys (long ys)

equivalentes' :: [Integer] -> [Integer] -> Integer -> Bool
equivalentes' [] [] _ = True
equivalentes' _ _ 0 = False
equivalentes' (x:xs) (y:ys) n = (x:xs) == (y:ys) || equivalentes' (x:xs) (ys ++ [y]) (n-1)

-- Ejercicio 4

eEquivalente :: Integer -> Integer -> [[Integer]]
eEquivalente n k = eliminarRepetidos (eEquivalente' (e n k) (long (e n k)))

eEquivalente' :: [Integer] -> Integer -> [[Integer]]
eEquivalente' _ 0 = []
eEquivalente' (x:xs) n | x == 1 = [(x:xs)] ++ (eEquivalente' (xs++[x]) (n-1))
                       | otherwise = eEquivalente' (xs++[x]) (n-1)

eliminarRepetidos :: [[Integer]] -> [[Integer]]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | elem x xs = eliminarRepetidos xs				   
                         | otherwise = x : eliminarRepetidos xs

-- Ejercicio 5

nkEquivalente :: [Integer] -> Bool
nkEquivalente xs = equivalentes xs (e (long xs) (cantUnos xs))

cantUnos :: [Integer] -> Integer
cantUnos [] = 0
cantUnos (x:xs) = x + cantUnos xs
