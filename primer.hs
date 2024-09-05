import System.Win32 (xBUTTON1)
import Data.Fixed (mod', div')
--ejercicio 1
--f:: Integer->Integer
--f 1 = 8
--f 4 = 131
--f 16= 16

--resuelto sin pattern matching:
--f2 :: Integer -> Integer
--f2 n | n == 1 = 8
   -- | n == 4 = 131
   -- | n == 16 = 16

--ejercicio b
--g:: Integer->Integer
--g 8 = 16
--g 16 = 4
--g 131= 1

--ejercicio c


--ejercicio 2
absoluto :: (Num a, Ord a) => a -> a
absoluto x | x >= 0    = x
           | otherwise = -x

--b maximo absoluto

maximoabsoluto :: (Num a, Ord a) => a -> a -> a
maximoabsoluto x y  | x >= y    = x
                    | otherwise  = y

--c maximo 3 devuelve maximo entre 3 numeros enteros

maximo3 :: (Num a, Ord a) => a -> a -> a -> a
maximo3 x y z | x >=y && x>=z  = x
              | y >=x && y>=z  = y       
              | otherwise  =z

-
--d)
--con pattern matching:
--algunoEs0 :: Float -> Float -> Bool
--algunoEs0 x 0 = True
--algunoEs0 0 y = True
--algunoEs0 x y = False
--sin pattern matching:
--algunoEs01 :: Float -> Float -> Bool
--algunoEs01 x y | x == 0 = True
  --             | y == 0 = True
    --           | otherwise = False 

--e)
--con pattern matching:
--ambosSon0 :: Float -> Float -> Bool
--ambosSon0 0 0 = True
--ambosSon0 x 0 = False
--ambosSon0 0 y = False
--ambosSon0 x y = False
--sin pattern matching:
--ambosSon01 :: Float -> Float -> Bool
--ambosSon01 x y | x == 0 && y == 0 = True
  --             | x /= 0 = False
    --           | y /= 0 = False


--h)
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False   

--i)
digitoUnidades :: Integer -> Integer
digitoUnidades x = mod (absoluto x) 10 --en vez de absoluto tambien se puede usar abs

--j)
digitoDecenas :: Integer -> Integer
digitoDecenas x = digitoUnidades (div (absoluto x) 10) --quita el ultimo numero de lo que se pone
                                                       --como entrada, y devuelve las decenas    


--ejercicio 3

estanRelacionados :: Integer ->Integer ->Bool
estanRelacionados   x y  
       |  x ==0|| y ==0  = False
       | (-x^2) `mod` (x * y) == 0 = True
       | otherwise = False


--ejercicio 4
--a)

prodInt :: (Int, Int) -> (Int, Int) ->(Int, Int)
prodInt (x,y)(z,w)= (x*z,y*w)
--b)
todomenor :: (Int, Int) -> (Int, Int) ->Bool
todomenor (x,y)(z,w)    
               | x < z = True
               | otherwise = False
             
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1,x2) (y1, y2) = sqrt ((x1 - y1)^2 + (x2 - y2)^2)

--d)
sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x, y, z) = x + y + z 



sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (a, b, c) d 
    | mod a d == 0 && mod b d == 0 && mod c d /= 0 = a + b  
    | mod a d /= 0 && mod b d == 0 && mod c d == 0 = b + c
    | mod a d == 0 && mod b d /= 0 && mod c d == 0 = a + c
    | mod a d == 0 && mod b d == 0 && mod c d == 0 = a + b + c
    | mod a d == 0 = a
    | mod b d == 0 = b 
    | mod c d == 0 = c
    | otherwise = 0        


posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x, y, z)
    | x `mod` 2 == 0 = x
    | y `mod` 2 == 0 = y
    | z `mod` 2 == 0 = z
    | otherwise = 4
--g)
crearPar :: a -> b -> (a, b) 
crearPar a b = ((a), (b))

--h)
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

todosmenores::(Integer, Integer, Integer) ->Bool
todosmenores (n1,n2,n3)=
     f n1 > g n1 && f n2 > g n2 && f n3 > g n3 
                   
-- Definición de la función g    
f :: Integer -> Integer
f n
        | n <= 7    = n^2  
        | otherwise = 3*n + 1

  
-- Definición de la función g
g :: Integer -> Integer
g n
      | n `mod` 2 == 0   = n `div` 2 
      |otherwise         =3*n+1   


    
--ejercicio 6

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio ->EsBisiesto
bisiesto x
         | x  `mod` 4 == 0 = True
         | otherwise = False


--ejercicio 7
distanciaManhattan:: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan (x,y,z)(v,w,f) = abs(x-v)+ abs(y-w)+abs(z-f)

--type Coordenada3d = (Float, Float, Float)

--distanciaManhattan :: Coordenada3d -> Coordenada3d -> Float
--distanciaManhattan (x1, y1, z1) (x2, y2, z2) = 
    --abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)


--distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float (otro ejemplo)
--distanciaManhattan (x, y, z) (v, w, f) = dx + dy + dz
  --where
    --dx = abs (x - v)
    --dy = abs (y - w)
    --dz = abs (z - f)                   


--ejercicio 8

comparar :: Integer ->Integer ->Integer
comparar x y | ultimosdigitosx < ultimosdigitosy = 1
             | ultimosdigitosx > ultimosdigitosy = -1
             | otherwise= 0
             where 
                ultimosdigitosx= ((abs x `mod`10) +(abs x `div` 10))`mod` 10
                ultimosdigitosy= ((abs y `mod` 10) +(abs y `div` 10))`mod` 10


--ejercicio 9
            