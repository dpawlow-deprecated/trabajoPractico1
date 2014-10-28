-- Ejercicio 1

long :: [a] -> Integer
long    []   = 0
long  (x:xs) = 1 + long xs

-- Ejercicio 2

-- En la primera coordenada de la tupla se forman los grupos, en la segunda quedan los elementos a repartir.
e :: Integer -> Integer -> [Integer]
e n k = cheqFinProceso ( (construirListaDe k 1 , construirListaDe (n-k) 0) )

{-

*Main> e 2 1
[1,0]

*Main> e 8 5
[1,0,1,1,0,1,0,1]

*Main> e 8 3
[1,0,0,1,0,1,0,0]

*Main> e 8 4
[1,0,1,0,1,0,1,0]


-}
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
{-
*Main> repartir ([[1],[1],[1]],[[0],[0],[0]])
([[1,0],[1,0],[1,0]],[])

*Main> repartir ([[1],[1],[1]],[[0],[0]])
([[1,0],[1,0]],[[1]])

*Main> repartir ([[1],[1],[1]],[[0],[0],[0],[0],[0]])
([[1,0],[1,0],[1,0]],[[0],[0]])

-}

desAgrupar :: [[Integer]] -> [Integer]
desAgrupar (x:[]) = x
desAgrupar (x:xs) = x ++ desAgrupar xs
{-
La función desagrupar concatena los grupos obtenidos dando el formato 
tipo [Integer] al resultado, que es lo pedido en el enunciado del ejercicio

*Main> desAgrupar [[1,0],[1,0],[1,0]]
[1,0,1,0,1,0]
*Main> desAgrupar [[1,0]]
[1,0]

-}
construirListaDe :: Integer -> Integer -> [[Integer]]
construirListaDe cont valor
               | cont == 0  = []
               | otherwise  = [valor] : construirListaDe (cont - 1) valor 
{-
Se usa para construir una lista de listas con un único [valor] repetido tantas
veces como indique (cont) que es un contador decreciente cargado incialmente con
la longitud de la lista de listas.

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

-- Al ser listas de 1 y 0, la suma de los elementos de la lista es la cantidad de unos.
cantidadDeUnos :: [Integer] -> Integer
cantidadDeUnos   []   = 0    
cantidadDeUnos (x:xs) = x + cantidadDeUnos xs

{-
*Main> nkEquivalente [1,1,1,0,0,0,1,1]
False
*Main> nkEquivalente [1,0,1,1,0,1,0,1]
True
*Main> nkEquivalente [1,1,0,1,0,1,1,0]
True

*Main> cantidadDe1s [1,0,1,1,0,1,0,1]
5
*Main> cantidadDe1s [0,0,0]
0
*Main> cantidadDe1s []
0
-}

------------------------------------THE--END------------------------------------
