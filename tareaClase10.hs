data  Lista a = Vacia | Agregar a (Lista a)
instance (Show a) => Show (Lista a) where
    show Vacia             = "[]"
    --show (Agregar x Vacia) = "[" ++ (show x) ++ "]"
    --show (Agregar x xs)    = "[" ++ (show x) ++ "," ++ (show' xs)
    show ls = "[" ++ show' ls ++ "]"

show' :: (Show a) => Lista a -> String
show' (Agregar x Vacia) = show x 
show' (Agregar x xs) = (show x) ++ "," ++ (show' xs)

long :: Lista a -> Integer
long Vacia = 0
long (Agregar x xs) = 1 + long xs

vacia :: Lista a -> Bool
vacia Vacia = True
vacia _     = False

suma :: Lista Float -> Float
suma Vacia          = 0
suma (Agregar a as) = a + suma as

enPosicion :: Lista a -> Integer -> a
enPosicion (Agregar x xs) 1             = x
enPosicion (Agregar x (Agregar y ys)) n = enPosicion (Agregar y ys) (n-1)

iguales :: Eq a => Lista a -> Lista a -> Bool
iguales Vacia          Vacia          = True
iguales Vacia          _              = False
iguales _              Vacia          = False
iguales (Agregar x xs) (Agregar y ys) = x == y && iguales xs ys

juntar :: Lista a -> Lista b -> Lista (a,b)
juntar (Agregar x Vacia) (Agregar y Vacia) = Agregar (x,y) Vacia
juntar (Agregar x xs)    (Agregar y ys)    = Agregar (x,y) (juntar xs ys)

-- Prueba de concat
(!) :: a -> Lista a -> Lista a
(!) x (Agregar h ls) = Agregar x (Agregar h ls) 