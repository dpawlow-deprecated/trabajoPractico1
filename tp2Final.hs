data Proposicion = P
                 | Q
                 | R
                 | No Proposicion
                 | Y Proposicion Proposicion
                 | O Proposicion Proposicion
                 | Imp Proposicion Proposicion deriving Eq

instance Show Proposicion where
        -- Ejercicio 2:
        show P = "P"
        show Q = "Q"
        show R = "R"
        show (No a)    = "~" ++ (show' a)
        show (Y a b)   = (show' a) ++ " ^ " ++ (show' b)  
        show (O a b)   = (show' a) ++ " v " ++ (show' b)  
        show (Imp a b) = (show' a) ++ " => " ++ (show' b)

show' :: Proposicion -> String
show' a | atomoONegacion a = show a
        | otherwise        = "(" ++ (show a) ++ ")"

-- Ejercicio 1:
atomoONegacion :: Proposicion -> Bool
atomoONegacion (No a) = True
atomoONegacion a      = a == P || a == Q || a == R

-- Ejercicio 3:
eliminarImplicaciones :: Proposicion -> Proposicion
eliminarImplicaciones (Imp a b) = O (No (eliminarImplicaciones a)) (eliminarImplicaciones b)
eliminarImplicaciones (No a)    = No (eliminarImplicaciones a)
eliminarImplicaciones (O a b)   = O (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones (Y a b)   = Y (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones a = a

-- Ejercicio 4:
aFNN :: Proposicion -> Proposicion
aFNN a = aFN (eliminarImplicaciones a)

aFN :: Proposicion -> Proposicion
aFN (No (Y a b))  = (O (aFN (No a)) (aFN (No b)))
aFN (No (O a b))  = (Y (aFN (No a)) (aFN (No b)))
aFN (No (No a) )  = (aFN a)
aFN (No a)        = (No (aFN a))
aFN (Y a (O b c)) = (O (aFN (Y a b)) (aFN (Y a c)))--opcional
aFN (Y a b) = (Y (aFN a) (aFN b))
aFN (O a b) = (O (aFN a) (aFN b))
aFN a = a 

-- Ejercicio 5:
evaluar :: Proposicion -> (Bool, Bool, Bool) -> Bool
evaluar P (p,q,r) = p
evaluar Q (p,q,r) = q
evaluar R (p,q,r) = r
evaluar (No a)  (p,q,r) = not(evaluar a (p,q,r))
evaluar (Y a b) (p,q,r) = (evaluar a (p,q,r)) && (evaluar b (p,q,r))
evaluar (O a b) (p,q,r) = (evaluar a (p,q,r)) || (evaluar b (p,q,r))
evaluar prop    (p,q,r) = evaluar (eliminarImplicaciones prop) (p,q,r)

-- Ejercicio 6:
combinacion :: Integer -> (Bool, Bool, Bool)
combinacion n = (1 == (div (mod n 8) 4), 1 == (mod (div n 2) 2), 1 == (mod n 2))

-- Ejercicio 7:
data TipoFormula = Tautologia | Contradiccion | Contingencia deriving Show

tipoDeFormula:: Proposicion -> TipoFormula
tipoDeFormula prop | (tests prop 7 True ) == True = Tautologia
                   | (tests prop 7 False) == True = Contradiccion
                   | otherwise                    = Contingencia

tests :: Proposicion -> Integer -> Bool -> Bool
tests prop n bool | n == 0    = (bool == evaluar prop (combinacion 0))
                  | otherwise = (bool == evaluar prop (combinacion n)) && tests prop (n-1) bool


-- Ejemplos:

{-
--Ej 1:
*Main> atomoONegacion (No (Q `O` R))
True
*Main> atomoONegacion  (Q `O` R)
False
*Main> atomoONegacion  R
True

--Ej 2:
*Main> No (No (Y (Imp R P) (No (O Q R))))
~~((R => P) ^ ~(Q v R))

-- Ej 3:
*Main> eliminarImplicaciones (Imp P (Imp Q (Y R Q)))
~P v (~Q v (R ^ Q))

-- Ej 4:
*Main> aFNN (No (Y (Imp R P) (No (No (O Q R)))))
(R ^ ~P) v (~Q ^ ~R)

-- Ej 5:
*Main> evaluar (P `Imp` Q `Imp` R) (True, False, False)
True
*Main> evaluar (P `Imp` R) (True, True, False)
False
*Main> evaluar (No (Y (Imp R P) (No (No (O Q R))))) (True, False, True)
False

-- Ej 6:
*Main> combinacion 0
(False,False,False)
*Main> combinacion 5
(True,False,True)
*Main> combinacion 1
(False,False,True)
*Main> combinacion (-38521)
(True,True,True)

-- Ej 7:
*Main> tipoDeFormula (Imp P Q)
Contingencia
*Main> tipoDeFormula (Imp P P)
Tautologia
*Main> tipoDeFormula (Q `Y` No Q)
Contradiccion

-}
-- Algunos ejemplos que pueden usar para probar las funciones
ejemplo1 = (O P P)  `Imp` (No (Y Q R)) -- (P ∨ P ) ⇒ ¬(Q ∧ R)
ejemplo2 = No (Y (Imp R P) (No (O Q R)))
ejemplo3 = No (No (Y (Imp R P) (No (O Q R))))
ejemplo4 = No (Y (Imp R P) (No (No (O Q R))))
ejemplo5 = ejemplo1 `Imp` ejemplo4

ejemplo6 = Y ejemplo5 (O ejemplo4 ejemplo3)
ejemplo7 = ejemplo6 `Imp` ejemplo4

{-
*Main> aFNN ejemplo7
((((~P ^ ~P) v (~Q v ~R)) ^ ((~R v P) ^ (Q v R))) v (((~R v P) ^ (Q v R)) ^ ((R ^ ~P) v (Q v R)))) v ((R ^ ~P) v (~Q ^ ~R))
*Main> eliminarImplicaciones ejemplo7
~((~(~(P v P) v ~(Q ^ R)) v ~((~R v P) ^ ~~(Q v R))) ^ (~((~R v P) ^ ~~(Q v R)) v ~~((~R v P) ^ ~(Q v R)))) v ~((~R v P) ^ ~~(Q v R))
*Main> ejemplo7
((((P v P) => ~(Q ^ R)) => ~((R => P) ^ ~~(Q v R))) ^ (~((R => P) ^ ~~(Q v R)) v ~~((R => P) ^ ~(Q v R)))) => ~((R => P) ^ ~~(Q v R))
*Main> tipoDeFormula ejemplo7
Tautologia
-}
