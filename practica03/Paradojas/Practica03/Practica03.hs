-- *Nombre del equipo: Paradojas*
--Liprandi Cortes Rodrigo 317275605
--Tinoco Miguel Laura Itzel 316020189
-- *Práctica 03*

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
fnc p = distrDNF (fnn p)

--Auxiliar, función que distribuye disyunciones de una fórmula
distrDNF:: Prop -> Prop
distrDNF (POr (PAnd a b) c) = distrDNF (PAnd (POr (distrDNF a) (distrDNF c)) (POr (distrDNF b) (distrDNF c)))
distrDNF (POr c (PAnd a b)) = distrDNF (PAnd (POr (distrDNF c) (distrDNF a)) (POr (distrDNF c) (distrDNF b)))
distrDNF (PAnd a b) = (PAnd (distrDNF a) (distrDNF b))
distrDNF f = f

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
unit (m, ((x:xs):ys)) = if length (x:xs) == 1 then (aConjunto([x] ++ m), ys) else uneAux [(x:xs)] (unit (aConjunto(m),ys))

-- 4. elim. Función que aplica la regla de eliminación. 
elim :: Solucion -> Solucion
elim (m, []) = (m,[])
elim ([], ys) = ([], ys)
elim ((x:xs), ys) = ((x:xs), elimAux x ys)

--Auxiliar para elim. Función que dada una literal y una fórmula elimina las clausulas donde aparezca esa literal
elimAux :: Literal -> [Clausula] -> [Clausula]
elimAux l [] = []
elimAux l (x:xs) = if((length x > 1) && (esta l x)) then (elimAux l xs) else [x] ++ (elimAux l xs)

-------------------------------------------------------------------------------------------------------------------
--Auxilar para Red. Elimina una literal de una clausula si contiene la negación de esta.
--redAux :: Literal -> Clausula -> Clausula
--redAux p [] = []
--redAux p (x:xs) = if x == simplificaNeg(PNeg p) then redAux p xs else ([x] ++ (redAux p xs))

--Auxiliar, usamos redAux para eliminar la contraria de una literal de una formula
--redAux2 :: Literal -> [Clausula] -> [Clausula]
--redAux2 x [] = []
--redAux2 x (y:ys) = [redAux x y] ++ (redAux2 x ys)

--Auxiliar, se usan nuestros dos auxiliares anteriores para lograr eliminar las contrarias de
-- una lista de literares en una lista de clausulas
--redAux3 :: [Literal] -> [Clausula] -> [Clausula]
--redAux3 [] [] = []
--redAux3 [] ys = []
--redAux3 xs [] = []
--redAux3 (x:xs) (y:ys) = [redAux x y] ++ (redAux2 x ys) ++ (redAux3 xs ys)

--Auxiliar
--uneSoluciones :: Solucion -> Solucion -> Solucion
--uneSoluciones (xs,ys) (xz,yz) = (xs, (ys ++ yz))
----------------------------------------------------------------------------------------------------------------------

--Auxiliar. Elimina una literal de una clausula.
--Como no queremos una clausula vacia, (conflict no checa si hay clausulas vacias), entonces si la
--clausula sólo tiene una literal, regresamos la misma clausula 
elimLiteral :: Literal -> Clausula -> Clausula
elimLiteral p [] = []
elimLiteral p [x] = [x]
elimLiteral p (x:xs) = if p == x then elimLiteral p xs else [x] ++ (elimLiteral p xs) 

--Auxiliar para red. Dada una literal y una fórmula, quita la contraria de la literal de todas las
--clausulas de la fórmula.
redAux :: Literal -> [Clausula] -> [Clausula]
redAux p [] = []
redAux p (x:xs) = if ((length x) > 1) && (esta (simplificaNeg(PNeg p)) x) 
                    then [elimLiteral (simplificaNeg(PNeg p)) x] ++ (redAux p xs) 
                  else [x] ++ (redAux p xs)

-- 5. red. Función que aplica la regla de reducción.
-- Usando los auxiliares, si la negación de la cabeza del modelo está en alguna clausula que tenga más
-- de una literal, entonces la eliminamos de esa clausula.
red :: Solucion -> Solucion
red (m, []) = (m,[])
red ([], ys) = ([], ys)
red (x:xs, ys) = (x:xs, redAux x ys)

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, []) = [(m,[])]
split (m, x:xs) = [([head x] ++ m, x:xs), ([simplificaNeg(PNeg(head x))] ++ m, x:xs)]

-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
-- Checamos en cada clausula: si la clausula sólo tiene una literal y esta es la negación de la cabeza
--                            del modelo, entonces regresamos True.
conflict :: Solucion -> Bool
conflict (m, []) = False
conflict ([], f) = False
conflict ((x:xs), (y:ys)) = ((length y) == 1) && (esta (simplificaNeg(PNeg x)) y) || conflict ((x:xs), ys) 

-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, f) = (f == [])

--9. appDPLL. Función que aplica las reglas anteriores una vez.
--Primero se aplica unit, despues elim y por ultimo red.
appDPLL :: Solucion -> Solucion
appDPLL (m, f) = red (elim (unit((m,f))))



{-- Puntos Extra --}
{--
--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = error "Sin implementar."
--}
