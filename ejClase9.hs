data ProgresionAritmetica = Vacio | CongruentesA Integer Integer
instance Show ProgresionAritmetica where
    show Vacio = "Vacio"
    show (CongruentesA x d) = "Una progresion de " ++ (show x) ++ " en " ++ (show d) 
instance Eq ProgresionAritmetica where
	(==) p q = (iguales p q) 

esMultiplo :: Integer -> Integer -> Bool
esMultiplo a 0 = a == 0
esMultiplo n m = mod n m == 0

pertenece :: Integer -> ProgresionAritmetica -> Bool
pertenece _ Vacio = False
pertenece n (CongruentesA a m) = esMultiplo (n - a) m

incluido :: ProgresionAritmetica -> ProgresionAritmetica -> Bool
incluido Vacio _ = True
incluido _ Vacio = False
incluido (CongruentesA a n) (CongruentesA b m) = esMultiplo m n && pertenece a (CongruentesA b m)

iguales :: ProgresionAritmetica -> ProgresionAritmetica -> Bool
iguales q p = incluido p q && incluido q p
--iguales (CongruentesA a n) (CongruentesA b m) = abs n == abs m && mod a n == mod b n



