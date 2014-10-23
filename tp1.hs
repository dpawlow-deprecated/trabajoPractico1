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
e' xs ys | long xs >= long ys = e' (agrupar (mezclar (take' (long ys) xs) ys)) (drop' (long ys) xs)
         | otherwise = e' (agrupar (mezclar xs (take' (long xs) ys))) (drop' (long xs) ys)

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
equivalentes xs ls = equivalentes' xs ls (long ls)

equivalentes' :: [Integer] -> [Integer] -> Integer -> Bool
equivalentes' _ _ 0 = False
equivalentes' (x:xs) (y:ys) n = (x:xs) == (y:ys) || equivalentes' (x:xs) (ys ++ [y]) ((long (y:ys))-1)

-- Ejercicio 4

eEquivalente :: Integer -> Integer -> [[Integer]]
eEquivalente n k = eliminarRepetidos (eEquivalente' (e n k) (long (e n k)))

eEquivalente' :: [Integer] -> Integer -> [[Integer]]
eEquivalente' _ 0 = []
eEquivalente' (x:xs) n | x == 1 = [(x:xs)] ++ (eEquivalente' (xs++[x]) (n-1))
					   | otherwise = eEquivalente' (xs++[x]) (n-1)

eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | elem x xs = eliminarRepetidos xs				   
                         | otherwise = x : eliminarRepetidos xs

-- Ejercicio 5

nkEquivalente :: [Integer] -> Bool
nkEquivalente xs = equivalentes xs (e (long xs) (nkEquivalente' xs))

nkEquivalente' :: [Integer] -> Integer
nkEquivalente' [] = 0
nkEquivalente' (x:xs) = x + nkEquivalente' xs