-- *Nombre del equipo/alumno*
--
--Tinoco Miguel Laura Itzel 316020189
-- *Práctica*

module Practica01 where 

--1
--primitivo. Función que recibe un entero y devuelve su primitivo.
--Ocupamos las operaciones mod y div para dividir los números en digitos
primitivo :: Int -> Int
primitivo n = if n < 10 then n else primitivo((n `mod` 10)* primitivo(n `div` 10)) 

--2
--area. Función que recibe tres puntos y devuelve el área del 
--      triángulo formado.
area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area v1 v2 v3 = heron (ladosT v1 v2 v3)

--Auxiliar, calcula la distancia entre dos puntos
distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2 -y1)^2)

--Auxiliar, regresa la longitud de los lados de un triangulo a partir de los vertices
ladosT :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double, Double)
ladosT v1 v2 v3 =
                 let a = distancia v1 v2
                     b = distancia v2 v3
                     c = distancia v1 v3
                 in (a,b,c)

--Auxiliar, calcula la formula de Heron dados la longitud de los lados del triangulo
heron :: (Double, Double, Double) -> Double
heron (x, y, z) = 
                 let s = (x + y + z)/2
                     a = s-x
                     b = s-y
                     c = s-z
                 in sqrt(s*a*b*c)

--3
--heterograma. Función que recibe una cadena y lo convierte en un
--             heterograma.
heterograma :: String -> String
heterograma [] = []
heterograma (x:xs) = if esta x xs then [x] ++ heterograma(quita x xs) else [x] ++ heterograma xs 

--4
--bolsa. Función que recibe una cadena y devuelve una lista de tuplas
--       con el número de ocurrencias de cada letra de la palabra.
bolsa :: String -> [(Char, Int)]
bolsa [] = []
bolsa (x:xs) = [(x,apariciones x (x:xs))] ++ bolsa (quita x xs)

--Auxiliar, elimina todas las apariciones de un elemento de una lista
quita :: Eq a => a -> [a] -> [a]
quita a [] = []
quita a (x:xs) = if a == x then quita a xs else [x] ++ quita a xs  

--Auxiliar, nos dice si un elemento esta en una lista
esta :: Eq a => a -> [a] -> Bool
esta _ [] = False
esta y (x:xs) = y==x || esta y xs   

--Auxiliar, nos dice el numero de apariciones de un elemento en una lista
apariciones :: Eq a => a -> [a] -> Int
apariciones a [] = 0
apariciones a (x:xs) = if a == x then 1 + apariciones a xs else 0 + apariciones a xs

--5
--esPalindromo. Función que verifica si una cadena es palíndromo.
esPalindromo :: Eq a => [a] -> Bool
esPalindromo s = if(s) == reversa s then True else False

--Auxiliar, regresa la reversa de una lista
reversa :: Eq a => [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--6
--diferencia. Función que devuelve una lista con la diferencia entre
--            dos listas.
--usamos la funcion elem que nos dice si un elemento pertenece a una lista
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] l2 = []
diferencia (x:xs) l2 = if elem x l2 then (diferencia xs l2) else [x] ++ (diferencia xs l2)

--7 
--primos. Función que devuelve una lista con todos los números primos
--        hasta n.
primos :: Int -> [Int]
primos n = criba[2..n]

--Auxiliar, método de criba para encontrar números primos
--Fuente: https://es.wikipedia.org/wiki/Criba_de_Erat%C3%B3stenes
criba :: [Int] -> [Int]
criba [] = []
criba (x:xs) = [x] ++ criba [y | y <- xs, (y `mod` x) /= 0]

{-- Definición de Binario.--}
data Binario = U | Cero Binario | Uno Binario

--8
--Instancia de la clase Show para Binario.
instance Show Binario where
        show U = "1"
        show (Cero b) = show b ++ "0"
        show (Uno b) = show b ++ "1"

--9
--suma. Función que devuelve la suma de dos Binarios.
suma :: Binario -> Binario -> Binario
suma U U = (Cero U)
suma U (Cero b) = (Uno b)
suma (Cero b) U = (Uno b)
suma U (Uno b) = (Cero (suma U (b)))
suma (Uno b) U = (Cero (suma U (b)))
suma (Uno b1) (Uno b2) = (Cero (suma U (suma b1 b2)))
suma (Cero  b1) (Cero b2) = (Cero (suma b1 b2))
suma (Cero b1) (Uno b2) = (Uno (suma b1 b2))
suma (Uno b1) (Cero b2) = (Uno (suma b1 b2))

{-- Definición del Árbol binario.--}
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving(Show)

--10
--inOrden. Función que convierte un árbol binario en una lista por
--         su recorrido in-orden.
inOrden :: Arbol a -> [a]
inOrden Vacio = []
inOrden (Nodo a i d) = (inOrden i) ++ [a] ++ (inOrden d)
