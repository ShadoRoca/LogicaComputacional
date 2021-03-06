-- *Nombre del equipo: Paradojas*
--Liprandi Cortes Rodrigo 317275605
--Tinoco Miguel Laura Itzel 316020189
-- *Práctica 04*

module Practica04 where

--Definición del tipo de datos para términos.
data Term = V Nombre | F Nombre [Term]

--Definición del tipo de datos para fórmulas.
data Form = NForm | TrueF | FalseF | Pr Nombre [Term] | Eq Term Term | 
            Neg Form | Conj Form Form | Disy Form Form | 
            Imp Form Form | Equi Form Form | All Nombre Form | 
            Ex Nombre Form

type Nombre = String

type Subst = [(Nombre,Term)]


--Instancia Show para Term.
instance Show Term where
  show (V x) = x
  show (F f t) = f ++ "(" ++ show t ++ ")"

--Instancia Show para Form.
instance Show Form where
  show NForm = ""
  show TrueF = "T"
  show FalseF = "F"
  show (Pr p t) = p ++ "(" ++ show t ++ ")"
  show (Eq t1 t2) = "(" ++ show t1 ++ "=" ++ show t2 ++ ")"
  show (Neg f) = "¬" ++ show f
  show (Conj f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
  show (Disy f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Imp f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
  show (Equi f1 f2) = "(" ++ show f1 ++ " <--> " ++ show f2 ++ ")"
  show (All x f) = "Alle " ++ x ++ " (" ++ show f ++ ")" 
  show (Ex x f) = "Ein " ++ x ++ " (" ++ show f ++ ")"



--1. alcance. Función que devuelve el alcance de los cuantificadores de
--          una fórmula.
alcance :: Form -> [(Form, Form)]
alcance TrueF = []
alcance FalseF = []
alcance (Pr x t) = []
alcance (All x f) = [((All x NForm), f)] ++ alcance f
alcance (Ex x f) = [((Ex x NForm), f)] ++ alcance f
alcance (Neg f) = alcance f
alcance (Conj f1 f2) = alcance (f1) ++ alcance (f2)
alcance (Disy f1 f2) = alcance (f1) ++ alcance (f2)
alcance (Imp f1 f2) = alcance (f1) ++ alcance (f2)
alcance (Equi f1 f2) = alcance (f1) ++ alcance (f2)

--2. bv. Función que devuelve las variables ligadas de una fórmula.
bv :: Form -> [Nombre]
bv TrueF = []
bv FalseF = []
bv (Pr x t) = []
bv (All x f) = [x] ++ (bv f)
bv (Ex x f) = [x] ++ (bv f)
bv (Neg f) = bv f
bv (Conj f1 f2) = bv (f1) ++ bv (f2)
bv (Disy f1 f2) = bv (f1) ++ bv (f2)
bv (Imp f1 f2) = bv (f1) ++ bv (f2)
bv (Equi f1 f2) = bv (f1) ++ bv (f2)

--Auxiliar para vars, nos da las variables de un solo termino
varsAux :: Term -> [Nombre]
varsAux (V n) = [n]
varsAux (F f []) = []
varsAux (F f (x:xs)) = (varsAux x) ++ varsAux (F f (xs))

--Auxiliar para vars, nos da las variables de una lista de terminos
varsAux2 :: [Term] -> [Nombre]
varsAux2 [] = []
varsAux2 (x:xs) = (varsAux x) ++ (varsAux2 xs)

--Auxiliar, nos da las variables de una formula
vars :: Form -> [Nombre]
vars TrueF = []
vars FalseF = []
vars (Pr x t) = varsAux2 t
vars (Eq t1 t2) = varsAux t1 ++ varsAux t2
vars (Neg f) = vars f
vars (Conj f1 f2) = vars f1 ++ vars f2
vars (Disy f1 f2) = vars f1 ++ vars f2
vars (Imp f1 f2) = vars f1 ++ vars f2
vars (Equi f1 f2) = vars f1 ++ vars f2
vars (All x f) = vars f
vars (Ex x f) = vars f

--Auxiliar, elimina todas las ocurrencias de un elemento de una lista
elimina :: Eq a => a -> [a] -> [a]
elimina x [] = []
elimina x (y:ys) = if x == y then elimina x ys else [y] ++ (elimina x ys)

--3. fv. Función que devuelve las variables libres de una fórmula.
fv :: Form -> [Nombre]
fv TrueF = []
fv FalseF = []
fv (Pr x t) = varsAux2 t 
fv (All x f) = elimina x (fv f)
fv (Ex x f) = elimina x (fv f)
fv (Eq t1 t2) = varsAux t1 ++ varsAux t2
fv (Neg f) = (fv f)
fv (Conj f1 f2) = fv f1 ++ fv f2
fv (Disy f1 f2) = fv f1 ++ fv f2
fv (Imp f1 f2) = fv f1 ++ fv f2
fv (Equi f1 f2) = fv f1 ++ fv f2

--4. sustTerm. Función que realiza la sustitución de variables en un término.
sustTerm :: Term -> Subst -> Term
sustTerm (V n) ((x,y):ys) = if n == x then y else sustTerm (V n) (ys)                    
sustTerm (F f l) t = let y = map (\x -> sustTerm x t) l
                         in (F f y)

-- Auxiliar para sustForm, aplica sustitución a una lista de términos
sustTermList :: [Term] -> Subst -> [Term]
sustTermList [] xs = []
sustTermList xs [] = xs
sustTermList (x:xs) (ys) =[sustTerm x ys] ++ (sustTermList xs ys)

--5. sustForm. Función que realiza la sustitución de variables en una 
--          fórmula sin renombramientos.
sustForm :: Form -> Subst -> Form
sustForm (f) [] = f
sustForm TrueF  s =  TrueF
sustForm FalseF  s = FalseF
sustForm (Pr x (y:ys)) s = (Pr x ([sustTerm y s] ++ (sustTermList ys s))) 
sustForm (All x f) s =  (All x (sustForm f s))
sustForm (Ex x f) s = (Ex x (sustForm f s))
sustForm (Eq t1 t2) s = (Eq (sustTerm t1 s) (sustTerm t2 s))
sustForm (Neg f) s = (Neg (sustForm f s))
sustForm (Conj f1 f2) s = (Conj (sustForm f1 s) (sustForm f2 s))
sustForm (Disy f1 f2) s = (Disy (sustForm f1 s) (sustForm f2 s))
sustForm (Imp f1 f2) s = (Imp (sustForm f1 s) (sustForm f2 s))
sustForm (Equi f1 f2) s = (Equi (sustForm f1 s) (sustForm f2 s))

--6. alphaEq. Función que dice si dos fórmulas son alpha-equivalentes.
alphaEq :: Form -> Form -> Bool
alphaEq f1 f2 = igual (fv f1) (fv f2) && igualForm f1 f2

--Auxiliar que nos dice si dos Form son iguales sin importar el nombre de variables o funciones
igualForm :: Form -> Form -> Bool
igualForm TrueF TrueF = True
igualForm FalseF FalseF = True
igualForm (Pr x t) (Pr n s)= comp t s
igualForm (All x f) (All n f2) = igualForm f f2
igualForm (Ex x f) (Ex n f2) = igualForm f f2
igualForm (Eq t1 t2) (Eq s1 s2) = igualTerm t1 s1 && igualTerm t2 s2
igualForm (Neg f) (Neg f2) = igualForm f f2
igualForm (Conj f1 f2) (Conj s1 s2) = igualForm f1 s1 && igualForm f2 s2
igualForm (Disy f1 f2) (Disy s1 s2) = igualForm f1 s1 && igualForm f2 s2
igualForm (Imp f1 f2) (Imp s1 s2) = igualForm f1 s1 && igualForm f2 s2
igualForm (Equi f1 f2) (Equi s1 s2) = igualForm f1 s1 && igualForm f2 s2
igualForm f s = False

--Auxiliar que nos dice si dos terminos son iguales sin importar el nombre
igualTerm :: Term -> Term -> Bool
igualTerm (V n) (V s) = True
igualTerm (V n) (F f l) = False
igualTerm (F f l) (V n)= False
igualTerm (F f l) (F n s) = comp l s

--Auxiliar que compara los elementos de dos listas uno a uno
comp :: [Term] -> [Term] -> Bool
comp [] [] = True
comp [] l = False
comp l [] = False
comp (x:xs) (y:ys) = igualTerm x y && comp xs ys


--Auxiliar que nos dice si dos listas son iguales.
igual :: [Nombre] -> [Nombre] -> Bool
igual [] [] = True
igual [] l = False
igual l [] = False
igual (x:xs) (y:ys) = length (x:xs) == length (y:ys) && (if x == y then (igual xs ys) else False)

