--
--
--

module Practica01 where 

primitivo :: Int -> Int
primitivo n = error "Sin implementar."

area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area v1 v2 v3 = error "Sin implementar."

heterograma :: String -> Bool
heterograma [] = []
heterograma (x:xs) = if (elem x xs) then (heterograma xs)
                                   else x:(heterograma xs)


bolsa :: Eq a => String -> [(a, b)]
bolsa s = error "Sin implementar."

esPalindromo :: String -> Bool
esPalindromo s = if s == reversa s then True else False

reversa :: String -> String
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]



remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) = if x == y then remove x ys
                                 else y:(remove x ys)

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) x [] = x
(\\) xs (y:ys) = if (elem y xs) then (remove y xs) \\ ys
                                     else xs \\ ys 


primos :: Int -> [Int]
primos n = [x | x <- [2..n], primo x]

primo :: Int -> Bool
primo n = factores n == [1, n]

factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]


data Binario = U | Cero Binario | Uno Binario

instance Show Binario where
    show U = "1"
    show (Cero b) = show b ++ "0"
    show (Uno b) = show b ++ "1"

suma :: Binario -> Binario -> Binario
suma U U = (Cero U)
--suma (Cero b1) (Cero b2) = (Cero suma (b1 b2))
--suma (Uno b1) (Uno b1) =  (Cero (suma (suma (U b1)) b2))


data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) deriving(Show)

inOrden :: Arbol a -> [a]
inOrden Vacio = []
inOrden (Nodo r i d) = inOrden i ++ [r] ++ inOrden d