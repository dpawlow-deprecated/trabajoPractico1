data Proposicion = P
                 | Q
                 | R
                 | No Proposicion
                 | Y Proposicion Proposicion
                 | O Proposicion Proposicion
                 | Imp Proposicion Proposicion deriving Eq
instance Show Proposicion where
  show P = "P"
  show Q = "Q"
  show R = "R"
  show (Y a b)   = (show' a) ++ " ^ " ++ (show' b)  
  show (O a b)   = (show' a) ++ " v " ++ (show' b)  
  show (Imp a b) = (show' a) ++ " => " ++ (show' b)
  show (No a)    = "~" ++ (show' a)

show' :: Proposicion -> String
show' a | atomoONegacion a = show a
        | otherwise = "(" ++ (show a) ++ ")"


-- Ej 1:

atomoONegacion :: Proposicion -> Bool
atomoONegacion (No x) = True
atomoONegacion x = x == P || x == Q || x == R

-- Ej 3

eliminarImplicaciones :: Proposicion -> Proposicion
eliminarImplicaciones (Imp a b) = O (No (eliminarImplicaciones a)) (eliminarImplicaciones b)
eliminarImplicaciones (No a)    = No (eliminarImplicaciones a)
eliminarImplicaciones (O a b)   = O (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones (Y a b)   = Y (eliminarImplicaciones a) (eliminarImplicaciones b)
eliminarImplicaciones x = x

-- where e = eliminarImplicaciones
--eliminarImplicaciones (a (b)) =
