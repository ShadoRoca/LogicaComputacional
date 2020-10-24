--
--
module Practica02 where

--Prop. Tipo de datos para proposiciones lógicas.
data Prop = PTrue | PFalse | PVar String | PNeg Prop | POr Prop Prop 
                  | PAnd Prop Prop | PImpl Prop Prop | PEquiv Prop Prop

--Estado. Lista de variables asignadas como verdaderas.
type Estado = [String]

--Instancia Show para Prop.
instance Show Prop where
  show PTrue = "true"
  show PFalse = "false "
  show (PVar x) = show x
  show (PNeg p) = "¬"++ (show p)
  show (POr p1 p2) = "(" ++ show p1 ++ " v " ++ show p2 ++ ")"
  show (PAnd p1 p2) = "(" ++ show p1 ++ " ^ " ++ show p2 ++ ")"
  show (PImpl p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
  show (PEquiv p1 p2) = "(" ++ show p1 ++ " <--> " ++ show p2 ++ ")"


--1. interp. Función que evalua una proposición dado el estado.
interp :: Estado -> Prop -> Bool
interp e PTrue = True
interp e PFalse = False
interp e (PVar x) = if esta x e then True else False
interp e (PNeg p) = if interp e p then False else True
interp e (POr p q) = (interp e p) || (interp e q)
interp e (PAnd p q) = (interp e p) && (interp e q)
interp e (PImpl p q) = not (interp e p) || (interp e q)
interp e (PEquiv p q) = ((interp e p) && (interp e q)) || (not (interp e p) && not (interp e q)) 

--Auxiliar, nos dice si un elemento está en una lista.
esta :: Eq a => a -> [a] -> Bool
esta _ [] = False
esta y (x:xs) = y==x || esta y xs

--2. estados. Función que devuelve una lista de todas las combinaciones
-- 				posibles de los estados de una proposición.
estados :: Prop -> [Estado]
estados p = error "Sin implementar."

--3. vars. Función que obtiene la lista de todas las variables de una
--			proposición.
vars :: Prop -> [String]
vars PTrue = []
vars PFalse = []
vars (PVar x) = [x]
vars (PNeg p) = vars p
vars (POr p q) = (vars p) ++ (vars q)
vars (PAnd p q) = (vars p) ++ (vars q)
vars (PImpl p q) = (vars p) ++ (vars q)
vars (PEquiv p q) = (vars p) ++ (vars q)

--4. subconj. Función que devuelve el conjunto potencia de una lista.
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = subconj xs ++ [(x:z) | z <- subconj xs]

--5. modelos. Función que devuelve la lista de todos los modelos posibles
-- 				para una proposición.
modelos :: Prop -> [Estado]
modelos p = modelosAux (subconj(vars p)) p

--Auxiliar para modelos, si un estado de todos los posibles estados de p satisfacen a p, entonces lo agrega a una lista 
modelosAux :: [[String]] -> Prop -> [Estado]
modelosAux [] p = []
modelosAux (x:xs) p = if (interp x p) then [x] ++ modelosAux xs p else modelosAux xs p

--6. tautologia. Función que dice si una proposición es tautología.
tautologia :: Prop -> Bool
tautologia p = error "Sin implementar."

--7. satisfen. Función que resuelve si una proposición es satisfacible
-- 				con cierto estado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = interp e p

--8. satisf. Función que resuelve si una proposición es satisfacible.
satisf :: Prop -> Bool
satisf p = error "Sin implementar."

--9. insatisfen. Función que resuelve si una proposición es insatisfacible
-- 					con cierto estado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = not (interp e p)

--10. contrad. Función que dice si una proposición es una contradicción.
contrad :: Prop -> Bool
contrad p = error "Sin implementar."

--11. equiv. Función que devuelve True si dos proposiciones son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = if (diferencia (modelos p1) (modelos p2)) == [] then True else False 

--Auxiliar para equiv, si ambas proposiciones son satisfacidas por los mismo modelos entonces la diferencia
--de las listas de sus modelos es igual a [], si no entonces no son satisfacidas por los mismos modelos
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] xs = []
diferencia (x:xs) ys = if esta x ys
                       then diferencia xs ys
                       else (x:(diferencia xs ys))

--12. elimEquiv. Función que elimina las equivalencias lógicas.
elimEquiv :: Prop -> Prop
elimEquiv p = error "Sin implementar."

--13. elimImpl. Función que elimina las implicaciones lógicas.
elimImpl :: Prop -> Prop
elimImpl p = error "Sin implementar."

--14. deMorgan. Función que aplica las leyes de DeMorgan a una proposición.
deMorgan :: Prop -> Prop
deMorgan p = error "Sin implementar."


{-- Punto extra--}
{--
estadosConj :: [Prop] -> [Estado]
modelosConj :: [Prop] -> [Estado]
satisfenConj:: Estado -> [Prop] -> Bool
satisfConj:: [Prop] -> Bool
insatisfenConj:: Estado -> [Prop] -> Bool
insatisfConj:: [Prop] -> Bool

--consecuencia. Función que determina si una proposición es consecuencia
--				del conjunto de premisas.
consecuencia: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi : gamma),
								satisfenConj i gamma,
								not (satisfen i phi)]

--argCorrecto. Función que determina si un argumento es lógicamente
--				correcto dadas las premisas.
argCorrecto :: [Prop] -> Prop -> Bool
argCorrecto gamma psi = consecuencia gamma psi
--}