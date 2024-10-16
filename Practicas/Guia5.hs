import System.Win32.Console (COORD(xPos, yPos))
import Test.HUnit
--ejercicio 1

longitud :: [t] -> Integer
longitud [] =0
longitud(__:xs)= 1+ longitud xs

ultimo :: [t] -> t
ultimo [t] = t
ultimo(_:xs)=  ultimo xs 

principio :: [t] -> [t]
principio (x:[]) = []
principio (x:xs) = x : principio xs

reverso :: [t] -> [t] 
reverso [] = []
reverso xs = ultimo xs : reverso (principio xs)



--ejercicio 2


pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
    | x == y    = True
    | otherwise = pertenece x ys


--todosIguales :: (Eq t) => [t] -> Bool
--todosIguales (x:[]) = True
--todosIguales (y:ys)
  --                |y /= todosigualesaux ys = False
    --              |otherwise= todosIguales ys
   -- where
    --    todosigualesaux :: [a]-> a 
      --  todosigualesaux (x:_) = x


todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:xs) = (x == y) && todosIguales (y:xs)



todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [_] = True
todosDistintos (x:xs)  
                |pertenece x xs = False
                |otherwise =todosDistintos xs


hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos xs  
                |todosDistintos xs = False
                |otherwise = True

                
quitar :: (Eq t) => t -> [t] -> [t]              
quitar _ [] = []
quitar x  (y:ys)
                 | x ==y = ys
                 | otherwise= y:quitar x ys


quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x ys
                 |pertenece x ys = quitarTodos x (quitar x ys)
                 |otherwise= ys


eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x:eliminarRepetidos (quitarTodos x xs)


mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True  -- Dos listas vacías tienen los mismos elementos
mismosElementos _ [] = False  -- Si la segunda lista es vacía y la primera no, es falso
mismosElementos [] _ = False  -- Si la primera lista es vacía y la segunda no, es falso
mismosElementos (x:xs) ys
    | pertenece x ys = mismosElementos xs (quitar x ys)
    | otherwise = False


capicua :: (Eq t) => [t] -> Bool
capicua []= True
capicua [x]= True
capicua xs 
          | reverso xs == xs = True
          | otherwise= False



--ejercicio 3
--1
sumatoria :: [Integer] -> Integer
sumatoria []=0
sumatoria (x:xs) = x + sumatoria xs
--2
productoria :: [Integer] -> Integer
productoria []=1
productoria (x:xs) = x * productoria xs
--3
maximo :: [Integer] -> Integer
maximo [x]=x
maximo (x:y:xs) 
               | x>=y = maximo (x:xs)
               |otherwise = maximo (y:xs)
--4
sumarN :: Integer -> [Integer] -> [Integer] 
sumarN x []=[]
sumarN  x (y:ys)=  (x+y): sumarN  x ys


--5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN primero xs
    where primero = head xs


--6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (ultimo xs) xs

--7
--pares :: [Integer] -> [Integer]


--8

--multiplosDeN :: Integer -> [Integer] -> [Integer]

--9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar xs = maximo xs : ordenar (quitar (maximo xs) xs)

--Ejercicio 4
--a
sacarRepetidos :: [Char] -> [Char]
sacarRepetidos [] = []
sacarRepetidos (x:[])=[x]
sacarRepetidos (x:y:xs) | x==y && x==' ' = sacarRepetidos(y:xs)
                               | otherwise = x:(sacarRepetidos(y:xs))

--b

empiezaConUnBlanco :: [Char] -> [Char] 
empiezaConUnBlanco [] = []  -- Manejo de lista vacía
empiezaConUnBlanco x | head x == ' ' = tail x
                     | otherwise = x


contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras l = contarPalabrasSinBlancos (empiezaConUnBlanco (sacarRepetidos l))

contarPalabrasSinBlancos :: [Char] -> Integer
contarPalabrasSinBlancos [] = 0
contarPalabrasSinBlancos (x:xs) 
    | x == ' '  = contarPalabrasSinBlancos (empiezaConUnBlanco xs)  -- Ignorar espacios
    | otherwise = 1 + contarPalabrasSinBlancos (quitarEspacios xs)
  
quitarEspacios :: [Char] -> [Char]
quitarEspacios (' ':ys) = quitarEspacios ys
quitarEspacios ys = ys  -- Si no hay más espacios, devuelve el resto

--c
palabras :: [Char] -> [[Char]]
palabras[]=[]
palabras (x:xs)= devolver (x:xs) :palabras (quitarEspacios(resto xs))
               

devolver :: [Char] -> [Char]
devolver [] = []
devolver(' ':_) = [] 
devolver (x:xs) = x : devolver xs 

resto :: [Char] -> [Char]
resto [] = []
resto (' ':xs) = xs  -- Devuelve el resto después del espacio
resto (_:xs) = resto xs  -- Sigue avanzando hasta encontrar un espacio


--palabraMasLarga :: [Char] -> [Char]



