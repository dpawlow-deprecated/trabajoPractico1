-- Ejercicio 1

long :: [a] -> Integer
long    []   = 0
long  (x:xs) = 1 + long xs

-- Ejercicio 2

e :: Integer -> Integer -> [Integer]
e n k = cheqFinProceso ( (construirListaDe k 1 , construirListaDe (n-k) 0) )

cheqFinProceso :: ([[Integer]],[[Integer]]) -> [Integer]
cheqFinProceso ( li , [] ) = desAgrupar li
cheqFinProceso ( li , ld ) = cheqFinProceso ( repartir ( li , ld ) )

repartir :: ([[Integer]],[[Integer]]) -> ([[Integer]],[[Integer]])
repartir ( (x:[]) , (y:[]) ) = ( [x ++ y] , [] )
repartir ( (x:xs) , (y:[]) ) = ( [x ++ y] , xs )
repartir ( (x:[]) , (y:ys) ) = ( [x ++ y] , ys )
repartir ( (x:xs) , (y:ys) ) = ( [x ++ y] ++ fst(repartir (xs, ys)) ,
                                             snd(repartir (xs, ys)) ) 

desAgrupar :: [[Integer]] -> [Integer]
desAgrupar (x:[]) = x
desAgrupar (x:xs) = x ++ desAgrupar xs

construirListaDe :: Integer -> Integer -> [[Integer]]
construirListaDe cont valor
               | cont == 0  = []
               | otherwise  = [valor] : construirListaDe (cont - 1) valor 

-- Ejercicio 3

equivalentes :: [Integer] -> [Integer] -> Bool
equivalentes xs ys = long xs == long ys && equivalentes' xs ys (long ys)

equivalentes' :: [Integer] -> [Integer] -> Integer -> Bool
equivalentes' []   []   _ = True
equivalentes' _    _    0 = False
equivalentes' xs (y:ys) n = xs == (y:ys) || equivalentes' xs (ys ++ [y]) (n-1)

-- Ejercicio 4

eEquivalente :: Integer -> Integer -> [[Integer]]
eEquivalente n k = eliminarRepetidos (eEquivalente' (e n k) (long (e n k)))

eEquivalente' :: [Integer] -> Integer -> [[Integer]]
eEquivalente'   _     0 = []
eEquivalente' (x:xs)  n 
            |  x == 1   = [(x:xs)] ++ (eEquivalente' (xs++[x]) (n-1))
            | otherwise =              eEquivalente' (xs++[x]) (n-1)

eliminarRepetidos :: [[Integer]] -> [[Integer]]
eliminarRepetidos   []  = []
eliminarRepetidos (x:xs) 
            | elem x xs =     eliminarRepetidos xs	   
            | otherwise = x : eliminarRepetidos xs

-- Ejercicio 5

nkEquivalente :: [Integer] -> Bool
nkEquivalente l = equivalentes l (e (long l) (cantidadDeUnos l) )

cantidadDeUnos :: [Integer] -> Integer
cantidadDeUnos   []   = 0    
cantidadDeUnos (x:xs) = x + cantidadDeUnos xs


------------------------------------THE--END------------------------------------
