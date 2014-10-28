-- Ejercicio 1

long :: [a] -> Integer
long    []   = 0
long  (x:xs) = 1 + long xs

{-
*Main> long [1,2,3,4,5]
5
-}


-- Ejercicio 2

-- Tupla: 1ra coordenada: se forman los grupos. 2da coord: quedan elementos a repartir.
e :: Integer -> Integer -> [Integer]
e n k = cheqFinProceso ( (construirListaDe k 1 , construirListaDe (n-k) 0) )

cheqFinProceso :: ([[Integer]],[[Integer]]) -> [Integer]
cheqFinProceso ( li , [] ) = desAgrupar li
cheqFinProceso ( li , ld ) = cheqFinProceso ( repartir ( li , ld ) )

-- Forma los grupos.
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

{-
*Main> e 8 5
[1,0,1,1,0,1,0,1]
*Main> e 8 3
[1,0,0,1,0,1,0,0]
*Main> e 8 4
[1,0,1,0,1,0,1,0]

*Main> repartir ([[1],[1],[1]],[[0],[0],[0],[0],[0]])
([[1,0],[1,0],[1,0]],[[0],[0]])
*Main> repartir ([[1,0],[1,0],[1,0]],[[0],[0]])
([[1,0,0],[1,0,0]],[[1,0]])

*Main> desAgrupar [[1,0],[1,0],[1,0]]
[1,0,1,0,1,0]

*Main> construirListaDe 1 5
[[1],[1],[1],[1],[1]]
*Main> construirListaDe 0 3
[[0],[0],[0]]
-}


-- Ejercicio 3

equivalentes :: [Integer] -> [Integer] -> Bool
equivalentes xs ys = long xs == long ys && equivalentes' xs ys (long ys)

equivalentes' :: [Integer] -> [Integer] -> Integer -> Bool
equivalentes' []   []   _ = True
equivalentes' _    _    0 = False
equivalentes' xs (y:ys) n = xs == (y:ys) || equivalentes' xs (ys ++ [y]) (n-1)

{-
*Main> equivalentes [1,2,3,4,5] [5,1,2,3,4]
True
*Main> equivalentes [1,0,1,0,1] [1,0,0,0,1]
False

*Main> equivalentes' [1,2,3] [3,1,2] (long [3,1,2])
True
-}


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

{-
*Main> eEquivalente 8 3
[[1,0,0,1,0,1,0,0],[1,0,1,0,0,1,0,0],[1,0,0,1,0,0,1,0]]
*Main> eEquivalente
eEquivalente   eEquivalente'
*Main> eEquivalente' [1,0,0,1] (long [1,0,0,1])
[[1,0,0,1],[1,1,0,0]]
-}


-- Ejercicio 5

nkEquivalente :: [Integer] -> Bool
nkEquivalente l = equivalentes l (e (long l) (cantidadDeUnos l) )

-- Al ser listas de 1 y 0, la suma de los elementos de la lista es la cantidad de unos.
cantidadDeUnos :: [Integer] -> Integer
cantidadDeUnos   []   = 0    
cantidadDeUnos (x:xs) = x + cantidadDeUnos xs

{-
*Main> nkEquivalente [1,1,1,0,0,0,1,1]
False
*Main> nkEquivalente [1,0,1,1,0,1,0,1]
True

*Main> cantidadDe1s [1,0,1,1,0,1,0,1]
5
*Main> cantidadDe1s [0,0,0]
0
-}

------------------------------------THE--END------------------------------------
