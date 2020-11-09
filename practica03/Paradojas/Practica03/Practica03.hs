--
--
--
module Practica03 where

import Practica02



{----- Formas Normales -----}

-- 1. fnn. Función que devuelve la Forma Normal Negativa de una 
--         proposición.
fnn :: Prop -> Prop
fnn p = simplificaNeg(elimImpl(elimEquiv p))

-- 2. fnc. Función que devuelve la Forma Normal Conjuntiva de una 
--         proposición.
fnc :: Prop -> Prop
fnc p = error "Sin implementar."



{----- Algoritmo DPLL -----}

-- Definiciones de algunos conceptos.
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

--Auxiliar para unit, une una lista de clausulas con la formula de una solucion
uneAux :: [Clausula] -> Solucion -> Solucion
uneAux xs (m,ys) = (m, ys ++ xs)

-- 3. unit. Función que aplica la regla unitaria.
unit :: Solucion -> Solucion
unit (m, []) = (m,[])
unit (m, ((x:xs):ys)) = if length (x:xs) == 1 then unit (aConjunto(m ++ [x]), ys) else uneAux [(x:xs)] (unit (aConjunto(m),ys))

-- 4. elim. Función que aplica la regla de eliminación. 
elim :: Solucion -> Solucion
elim (m, f) = error "Sin implementar."

--Auxilar para Red. Elimina una literal de una clausula si contiene la negación de esta.
redAux :: Literal -> Clausula -> Clausula
redAux p [] = []
redAux p (x:xs) = if x == simplificaNeg(PNeg p) then redAux p xs else ([x] ++ (redAux p xs))

--Auxiliar, usamos redAux para eliminar la contraria de una literal de una formula
redAux2 :: Literal -> [Clausula] -> [Clausula]
redAux2 x [] = []
redAux2 x (y:ys) = [redAux x y] ++ (redAux2 x ys)

--Auxiliar, se usan nuestros dos auxiliares anteriores para lograr eliminar las contrarias de
-- una lista de literares en una lista de clausulas
redAux3 :: [Literal] -> [Clausula] -> [Clausula]
redAux3 [] [] = []
redAux3 [] ys = []
redAux3 xs [] = []
redAux3 (x:xs) (y:ys) = [redAux x y] ++ (redAux2 x ys) ++ (redAux3 xs ys)

--
uneSoluciones :: Solucion -> Solucion -> Solucion
uneSoluciones (xs,ys) (xz,yz) = (xs, (ys ++ yz))

-- 5. red. Función que aplica la regla de reducción.
red :: Solucion -> Solucion
red (m, []) = (m,[])
red ([], ys) = ([], ys)
red (x:xs, ys) = (x:xs, redAux3 (x:xs) ys)

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, f) = error "Sin implementar."

--
vacia :: Clausula -> Bool
vacia [] = True
vacia xs = False

--
clausulaVacia :: [Clausula] -> Bool
clausulaVacia [] = False 
clausulaVacia [[]] = True

-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
conflict (m, f) = error "sin impl"

-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, f) = error "Sin implementar."

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL (m, f) = error "Sin implementar."



{-- Puntos Extra --}
{--
--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = error "Sin implementar."
--}