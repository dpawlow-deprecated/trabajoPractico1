data Polinomio = Mono Float Integer
               | Suma Polinomio Polinomio
               | Producto Polinomio Polinomio
instance Show Polinomio where
  show (Mono a n) = (show a) ++ " x^" ++ (show n)
  show (Suma p q) = show p ++ " + " ++ show q 
  show (Producto p q) = "(" ++ (show p) ++ ") * (" ++ (show q) ++ ")" 
instance Num Polinomio where
  p + q = Suma p q
  p * q = Producto p q
  negate p = opuesto p
  abs p = error "no hay por ahora"
  signum p = error "no hay por ahora"
  fromInteger n = Mono (fromInteger n) 0

eval :: Polinomio -> Float -> Float
eval (Mono a n) x = a * (x ^ n)
eval (Suma p1 p2) x = eval p1 x + eval p2 x 
eval (Producto p1 p2) x = (eval p1 x) * (eval p2 x)

coefs :: Polinomio -> [Float]
coefs (Mono a 0) = [a]
coefs (Mono a n) = 0 : coefs (Mono a (n-1))
coefs (Suma p q) = sumaListas (coefs p) (coefs q)
coefs (Producto p q) = productoListas (coefs p) (coefs q)

sumaListas :: [Float] -> [Float] -> [Float]
sumaListas [] ys = ys
sumaListas xs [] = xs
sumaListas (x:xs) (y:ys) = (x + y) : (sumaListas xs ys)

productoListas :: [Float] -> [Float] -> [Float]
productoListas [] _ = []
productoListas _ [] = []
productoListas (x:xs) ys = sumaListas xys xsys
      where xys = cteLista x ys
            xsys = 0 : productoListas xs ys

cteLista :: Float -> [Float] -> [Float]
cteLista n [] = []
cteLista n (x:xs) = (x * n) : (cteLista n xs)

opuesto :: Polinomio -> Polinomio
opuesto (Mono a n) = Mono (-a) n
opuesto (Suma p q) = Suma (opuesto p) (opuesto q)
opuesto (Producto p q) = Producto (opuesto p) q

eliminarCeros :: [Float] -> [Float]
eliminarCeros [] = []
eliminarCeros [0] = [0]
eliminarCeros xs | (last xs) == 0 = eliminarCeros (init xs)
                 | otherwise = xs

gradoPol :: Polinomio -> Int
gradoPol p = length (eliminarCeros (coefs p)) - 1