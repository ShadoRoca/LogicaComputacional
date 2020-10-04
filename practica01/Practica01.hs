-- *Nombre del equipo/alumno*
-- *Práctica*

module Practica01 where 

--1
--primitivo. Función que recibe un entero y devuelve su primitivo.
primitivo :: Int -> Int
primitivo n = error "Sin implementar."

--2
--area. Función que recibe tres puntos y devuelve el área del 
--      triángulo formado.
area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area v1 v2 v3 = error "Sin implementar."

--3
--heterograma. Función que recibe una cadena y lo convierte en un
--             heterograma.
heterograma :: String -> String
heterograma s = error "Sin implementar."

--4
--bolsa. Función que recibe una cadena y devuelve una lista de tuplas
--       con el número de ocurrencias de cada letra de la palabra.
bolsa :: Eq a => String -> [(Char, Int)]
bolsa s = error "Sin implementar."

--5
--esPalindromo. Función que verifica si una cadena es palíndromo.
esPalindromo :: Eq a => [a] -> Bool
esPalindromo s = error "Sin implementar."

--6
--diferencia. Función que devuelve una lista con la diferencia entre
--            dos listas.
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia l1 l2 = error "Sin implementar."

--7
--primos. Función que devuelve una lista con todos los números primos
--        hasta n.
primos :: Int -> [Int]
primos n = error "Sin implementar."



{-- Definición de Binario.--}
data Binario = U | Cero Binario | Uno Binario

--8
--Instancia de la clase Show para Binario.
instance Show Binario where
	show b = error "Sin implementar."

--9
--suma. Función que devuelve la suma de dos Binarios.
suma :: Binario -> Binario -> Binario
suma b1 b2 = error "Sin implementar."



{-- Definición del Árbol binario.--}
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving(Show)

--10
--inOrden. Función que convierte un árbol binario en una lista por
--         su recorrido in-orden.
inOrden :: Arbol a -> [a]
inOrden t = error "Sin implementar."
