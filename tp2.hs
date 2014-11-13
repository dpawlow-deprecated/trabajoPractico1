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
        show (Y a b)   = (show' a) ++ " ^ " ++ (show' b)  
        show (O a b)   = (show' a) ++ " v " ++ (show' b)  
        show (Imp a b) = (show' a) ++ " => " ++ (show' b)
        show (No a)    = "~" ++ (show' a)

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
eliminarImplicaciones x = x

-- Ejercicio 4:

--aFNN :: Proposicion -> Proposicion
--aFNN x = aFNN' (eliminarImplicaciones x)

deMorgan :: Proposicion -> Proposicion
deMorgan (No (Y a b)) = O (No (deMorgan a)) (No (deMorgan b))
deMorgan (No (O a b)) = Y (No (deMorgan a)) (No (deMorgan b))
deMorgan (O a b)     = O (deMorgan a) (deMorgan b)
deMorgan (Y a b)     = Y (deMorgan a) (deMorgan b)
deMorgan (Imp a b)   = Imp (deMorgan a) (deMorgan b)
deMorgan x = x



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
combinacion n = (1 == (div (div n 2) 2), 1 == (mod (div n 2) 2), 1 == (mod n 2))

-- Ejercicio 7:

data TipoFormula = Tautologia | Contradiccion | Contingencia deriving Show

tipoDeFormula:: Proposicion -> TipoFormula
tipoDeFormula prop | (tipo prop 7 True ) == True = Tautologia
                   | (tipo prop 7 False) == True = Contradiccion
                   | otherwise                   = Contingencia

tipo :: Proposicion -> Integer -> Bool -> Bool
tipo prop n bool | n == 0    = (bool == evaluar prop (combinacion 0))
                 | otherwise = (bool == evaluar prop (combinacion n)) && tipo prop (n-1) bool



























