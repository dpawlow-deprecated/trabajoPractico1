module Tp2 where


data Proposicion = P
                  | Q
                  | R
                  | No Proposicion
                  | Y Proposicion Proposicion
                  | O Proposicion Proposicion
                  | Imp Proposicion Proposicion deriving Eq





{-******************************************************************************
Ejercicio 1
    Dar el tipo y definir la función atomoONegacion, que dada una proposición, devuelve verdadero sólo en el caso que esta proposición sea una variable o la negación de una proposición.



******************************************************************************-}
atomoONegacion :: Proposicion -> Bool
atomoONegacion (No x) = True
atomoONegacion x = x == P || x == Q || x == R -- || x == (No x)-- || x == (No Q) || x == (No R)

negacion :: Proposicion -> Proposicion 
negacion x = (No x)
{-
*Tp2> atomoONegacion P
True
*Tp2> atomoONegacion Q
True
*Tp2> atomoONegacion R
True
*Tp2> atomoONegacion (No P)
True
*Tp2> atomoONegacion (No Q)
True
*Tp2> atomoONegacion (No R)
True
*Tp2> atomoONegacion (Y R Q)
False
*Tp2> atomoONegacion (O P Q)
False
*Tp2> atomoONegacion (Imp P Q)
False
-}
{-******************************************************************************
Ejercicio 2
    Implementar la función show para el tipo Proposicion, que retorna la representación textual de una fórmula proposicional. Sólo se deben agregar paréntesis alrededor de una sub-proposición cuando la misma no sea una variable o una negación.
Por ejemplo, la proposición:
                          
No (No (Y (Imp R P) (No (O Q R))))
debe verse como: ¬¬((R => P ) ∧ ¬(Q ∨ R))
                                                 
    Para los símbolos, utilicen los siguientes carácteres (no copien y peguen de acá porque puede traer errores)
      ~ (tilde) para la negación.
      ^ (acento circunflejo) para la conjunción.
      v (v corta) para la disyunción.
      => (igual, mayor) para la implicación.


******************************************************************************-}
instance Show Proposicion where
	show (P) = "P"
        show (Q) = "Q"
        show (R) = "R"
--        show (No (No x)) = "~~"++(show' x)
        show (No x) = "~"++(show' x)
        show (Y x y)   = (show' x)++" ^ " ++(show' y)
        show (O x y)   = (show' x)++" v " ++(show' y)
        show (Imp x y) = (show' x)++" => "++(show' y)
        
show' x = if atomoONegacion x then (show x) else "("++(show x)++")"
{-
*Tp2> No (No (Y (Imp R P) (No (O Q R))))
~~((R => P) ^ ~(Q v R))

        show (Y x y)   = "("++(show x)++" ^ " ++(show y)++")"
        show (O x y)   = "("++(show x)++" v " ++(show y)++")"
        show (Imp x y) = "("++(show x)++" => "++(show y)++")"
-}



{-******************************************************************************
Ejercicio 3
    Dar el tipo y definir la función eliminarImplicaciones, que dada una proposición retorna
una equivalente pero sin implicaciones. Recordar que A => B es equivalente a ¬A ∨ B.
    Ejemplo:
*Main> eliminarImplicaciones (Imp P (Imp Q (Y R Q)))
Deberá devolver: ¬P ∨ (¬Q ∨ (R ∧ Q))




******************************************************************************-}
eliminarImplicaciones :: Proposicion -> Proposicion
eliminarImplicaciones x = e x 

e :: Proposicion -> Proposicion
e (O a b)= (O (e a) (e b))
e (Y a b)= (Y (e a) (e b))
e (No x) = (No (e x))
e (Imp a b) = (O (No (e a)) (e b))
e (P) = P
e (Q) = Q
e (R) = R                                      

{-
*Tp2> eliminarImplicaciones (No (Y (Imp R P) (No (No (O Q R)))))
~((~R v P) ^ ~~(Q v R))
*Tp2> eliminarImplicaciones (Imp P (Imp Q (Y R Q)))
~P v (~Q v (R ^ Q))
-}


{-******************************************************************************
Ejercicio 4
    Nota: Este ejercicio tiene Prioridad Baja, dejar para el final del trabajo si queda tiempo.
El conjunto de las proposiciones en forma normal negada (FNN) se define inductivamente como:
      Las variables y sus negaciones están en FNN.
      Si φ, ψ ∈ FNN, entonces (φ ∨ ψ), (φ ∧ ψ) ∈ FNN.
Por ejemplo, P ∨ ¬P está en FNN, pero ¬(P ∧ ¬P ) no.
    En otras palabras, se trata de que las negaciones estén lo más adentro posible. (Notar también que no se admiten implicaciones).
    Dar el tipo y definir la función aFNN, que dada una fórmula retorna otra semánticamente
equivalente pero en forma normal negada. Se sugiere primero, eliminar las implicaciones y luego procesar la fórmula. Ejemplos de pasaje a forma normal negada:
aFNN (No (Y (Imp R P) (No (No (O Q R))))) es decir: aFNN (¬((R => P ) ∧ ¬¬(Q ∨ R)))
devuelve (R ∧ ¬P ) ∨ (¬Q ∧ ¬R)


******************************************************************************-}
aFNN' :: Proposicion -> Proposicion

aFNN' x = aFNN (eliminarImplicaciones x)
--aFNN (Imp x y) =  (O (No (aFNN x)) (aFNN y))
aFNN (Y x (O y z)) = (O (Y (aFNN x) (aFNN y)) (Y (aFNN x) (aFNN z)))
aFNN (No (Y x y)) = (O (No (aFNN x)) (No (aFNN y)))
aFNN (No (O x y)) = (Y (No (aFNN x)) (No (aFNN y)))
aFNN (No (No x) ) =   (aFNN x)
aFNN ( No x  ) =  (No (aFNN x))
aFNN ( Y  x y) =  (Y (aFNN x) (aFNN y))
aFNN ( O  x y) =  (O (aFNN x) (aFNN y))
--aFNN (No P) = (No P)
--aFNN (No Q) = (No Q)
--aFNN (No R) = (No R)
aFNN (P) = P
aFNN (Q) = Q
aFNN (R) = R

{-
*Tp2> aFNN' (No (Y (O (No R) P) (No (No (O Q R)))))
~(~R v P) v ~(Q v R)
*Tp2> aFNN' (No (Y (Imp R P) (No (No (O Q R)))))
~(~R v P) v ~(Q v R)

-}

{-******************************************************************************
Ejercicio 5
    Definir la función evaluar :: Proposicion ->(Bool, Bool, Bool) ->Bool , que retorne el
valor que representa un término dados los valores para P, Q y R.
    Por ejemplo:
*Main> evaluar (P ‘Imp‘ Q ‘Imp‘ R) (True, False, False)
True
*Main> evaluar (P ‘Imp‘ R) (True, True, False)
False
*Main> evaluar (No (Y (Imp R P) (No (No (O Q R))))) (True, False, True)
False



******************************************************************************-}
evaluar :: Proposicion -> (Bool, Bool, Bool) -> Bool
evaluar ( No x  ) (p,q,r) =  not(evaluar x (p,q,r))
evaluar ( Y  x y) (p,q,r) =  (evaluar x (p,q,r)) && (evaluar y (p,q,r))
evaluar ( O  x y) (p,q,r) =  (evaluar x (p,q,r)) || (evaluar y (p,q,r))
evaluar (Imp x y) (p,q,r) =  (not(evaluar x (p,q,r))) || (evaluar y (p,q,r))
evaluar prop (p,q,r)
  | prop == P = p
  | prop == Q = q
  | prop == R = r

{-
*Tp2> evaluar (No R) (True,True,False)
True
*Tp2> evaluar (No Q) (True,True,False)
False
*Tp2> evaluar (No (No (Y (Imp R P) (No (O Q R))))) (True,False,True)
False
*Tp2> evaluar (No (No (Y (Imp R P) (No (O Q R))))) (True,True,True)
False
*Tp2> evaluar (No (No (Y (Imp R P) (No (O Q R))))) (False,True,True)
False
*Tp2> evaluar (No (No (Y (Imp R P) (No (O Q R))))) (False,True,False)
False
*Tp2> evaluar (No (No (Y (Imp R P) (No (O Q R))))) (False,False,False)
True
-}



{-******************************************************************************
Ejercicio 6
   Definir la función combinacion :: Integer -> (Bool, Bool, Bool) que dado un número
entre 0 y 7 devuelva su representación en binario en forma de tripla. Es decir:

*Main> combinacion 0
(False,False,False)
*Main> combinacion 5
(True,False,True)
   Para este ejercicio no está permitido resolver el ejercicio por extensión (es decir, caso por caso).
Debería alcanzar con una sola linea utilizando las funciones matemáticas correspondientes.


******************************************************************************-}
combinacion :: Integer -> (Bool, Bool, Bool)
combinacion n = ( 1==(div (div n 2) 2),1==(mod (div n 2) 2),1==(mod n 2))

{-
*Tp2> combinacion 1
(False,False,True)
*Tp2> combinacion 2
(False,True,False)
*Tp2> combinacion 3
(False,True,True)
*Tp2> combinacion 4
(True,False,False)
*Tp2> combinacion 5
(True,False,True)
*Tp2> combinacion 6
(True,True,False)
*Tp2> combinacion 7
(True,True,True)
*Tp2> combinacion 8
(False,False,False)
*Tp2> combinacion 0
(False,False,False)

-}

{-******************************************************************************
Ejercicio 7
Dado el tipo data TipoFormula = Tautologia | Contradiccion | Contingencia deriving Show
Definir la función tipoDeFormula que retorne el tipo de formula de una proposición dada. Por
ejemplo,
*Main> tipoDeFormula (Imp P Q)
Contingencia
*Main> tipoDeFormula (Imp P P)
Tautologia
*Main> tipoDeFormula (Q ‘Y‘ No Q)
Contradiccion


******************************************************************************-}
-- Ejercicio 7
data TipoFormula = Tautologia | Contradiccion | Contingencia deriving Show

tipoDeFormula:: Proposicion -> TipoFormula
tipoDeFormula x 
  | (tipo x 7 True ) == True = Tautologia
  | (tipo x 7 False) == True = Contradiccion
  |               otherwise  = Contingencia
-- valor es True implica True (tautologico), valor False implica True (contradiccion),
-- si en ambos casos implica False entonces es contingencia n = 7 x es proposición
tipo x n valor
  | n == 0 = (valor == evaluar x (combinacion 0))
  | otherwise = (valor == evaluar x (combinacion n)) && tipo x (n-1) valor

{-
*Tp2> tipoDeFormula ejemplo5
Contingencia
*Tp2> tipoDeFormula (Y P (No P)) 
Contradiccion
*Tp2> tipoDeFormula (O P (No P)) 
Tautologia
*Tp2> 

-}



{-
no se usa
tipo2 x n valor
  | n == 0 = (valor == evaluar x (combinacion 0))
  | otherwise = (valor == evaluar x (combinacion n)) || tipo2 x (n-1) valor
-}


-- Algunos ejemplos que pueden usar para probar las funciones
ejemplo1 = (O P P)  `Imp` (No (Y Q R)) -- (P ∨ P ) ⇒ ¬(Q ∧ R)
ejemplo2 = No (Y (Imp R P) (No (O Q R)))
ejemplo3 = No (No (Y (Imp R P) (No (O Q R))))
ejemplo4 = No (Y (Imp R P) (No (No (O Q R))))
ejemplo5 = ejemplo1 `Imp` ejemplo4




