import System.Win32 (COORD(yPos))
import Data.Fixed (mod', div')
--ejercicio 1 finnovacii

fibonacci::Int->Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


--ejercicio 2 

parteEntera :: Float -> Int
parteEntera n 
    | n >= 0 && n < 1 = 0
    | n >= 1          = 1 + parteEntera (n - 1)
    | n <= 0 && n > -1 = 0
    | otherwise       = -1 + parteEntera (n + 1)



--ejercicio 3

esDivisible :: Integer -> Integer -> Bool
esDivisible x y
      | y == 0    = False  -- Evita división por cero
      | otherwise = esDivisiblePositivo (abs x) (abs y)  -- Usa los valores absolutos
             
  
 where
       esDivisiblePositivo :: Integer ->Integer ->Bool      
       esDivisiblePositivo x y
        | x ==0  = True
        | x < y  = False 
        | x == y = True
        | otherwise  = esDivisiblePositivo (x-y) y
        
   
--ejercicio 4
sumaImpares::Int->Int
sumaImpares 0 = 0
sumaImpares a = sumaImpares (a-1) + (a*2 - 1) 


--ejercicio 5
medioFact :: Integer ->Integer
medioFact 0 =1
medioFact 1 = 1
medioFact n= (n)* medioFact(n-2)

--ejercicio 6

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n
    | n < 10 = True  -- Un solo dígito siempre cumple la condición
    | rem n 10 == rem (div n 10) 10 = todosDigitosIguales (div n 10) -- Compara el último dígito con el penúltimo
    | otherwise = False



--ejercicio 7
iesimoDigito :: Integer ->Integer ->Integer
iesimoDigito n i = (n `div` 10^(cantDigitos(n) - i) )`mod` 10
                 
cantDigitos :: Integer ->Integer 
cantDigitos n 
       |n ==0     =1
       |n < 10    = 1
       |otherwise = 1 + cantDigitos (n `div` 10)

--ejercicio 8 
sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = (n `mod` 10) + sumaDigitos (n `div` 10)    

--ejercicio 9
esCapicua :: Integer -> Bool
esCapicua n | (n < 10) = True
            | (invertir (n) == n) = True
            | otherwise = False

invertir :: Integer -> Integer
invertir n | (cantDigitos  (n) == 1) = n
           | otherwise = (ultimoDigito (n) * 10^(cantDigitos (n)-1)) + invertir (sacarUltimo (n))
           where ultimoDigito n = mod n 10
                 sacarUltimo n = div n 10

--[10]
f1 :: Integer -> Integer
f1 0 = 1
f1 n = f1 (n-1) + 2^n


f2::Integer->Float->Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q


f3::Integer->Float->Float
f3 n  = f2 (2*n) 


--[11]

factorial :: Integer ->Integer
factorial  0 =1
factorial  n=  n *(factorial (n-1))
 
 
eAprox :: Integer ->Float 
eAprox 1 = 2
eAprox n = 1/fromIntegral(factorial n) + eAprox(n-1)


--[12]
an :: Integer ->Float
an 1 = 2
an n = 2 + 1/an (n-1)

raizDe2Aprox :: Integer ->Float
raizDe2Aprox 1 = 1
raizDe2Aprox n = an n-1


--[13]
sumatoriaDoble :: Integer->Integer->Integer
sumatoriaDoble 0 _ = 0
sumatoriaDoble n m = sumatoriaSimple n m + sumatoriaDoble (n-1) m 

sumatoriaSimple::Integer->Integer->Integer
sumatoriaSimple _ 0 = 0
sumatoriaSimple i j  = i^j + sumatoriaSimple i (j-1)
--problema sumatoriaDoble(n,m:Z):Z{
    --requiere {n>0}
    --requiere {m>0}
    --asegura {res=sum desde i=1 hasta n sum desde j=1 hasta m de i^j}

--[14]





--[15]
-- Función principal para calcular la suma doble
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 0 _ = 0
sumaRacionales n m = sumaRacionalesAux (n, m)

-- Función auxiliar para sumar las fracciones para cada valor de p
sumaRacionalesAux :: (Integer, Integer) -> Float
sumaRacionalesAux (0, _) = 0
sumaRacionalesAux (p, m) = sumaSimple p m + sumaRacionalesAux (p-1, m)

-- Suma de las fracciones para un valor fijo de p
sumaSimple :: Integer -> Integer -> Float
sumaSimple _ 0 = 0
sumaSimple p q = fromIntegral p / fromIntegral q + sumaSimple p (q-1)

--[16]

menorDivisorAux::Integer->Integer->Integer
menorDivisorAux a i  | mod a i == 0 =i
                     | otherwise = menorDivisorAux a (i+1)

menorDivisor::Integer->Integer
menorDivisor n = menorDivisorAux n 2


esPrimo::Integer->Bool
esPrimo n = n == menorDivisor n

--Algoritmo de Euclides
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (rem a b)

sonCoprimos::Integer->Integer->Bool
sonCoprimos n m = mcd n m == 1


nEsimoPrimo::Integer->Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = siguientePrimo (nEsimoPrimo (n-1))
-- Devuelve el primo nº n, 
-- Ej: nEsimoPrimo 4 = 7, nEsimoPrimo5=11 , nEsimoPrimo 19=67

siguientePrimo::Integer->Integer
siguientePrimo n | esPrimo (n+1) = n+1
                 | otherwise = siguientePrimo (n+1)
-- Si el siguiente numero es primo, devuelve ese numero, si no, vuelve a chequear hasta tener uno primo
-- Ej: siguientePrimo 3 = 5, siguientePrimo 5=7, siguientePrimo 6=7


esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciAux n 0

-- Función auxiliar que recorre los números de Fibonacci
esFibonacciAux :: Int -> Int -> Bool
esFibonacciAux n i
    | fibonacci i == n = True
    | fibonacci i > n  = False
    | otherwise        = esFibonacciAux n (i + 1)


--[18]
 mayorDigitoPar :: Integer ->Integer







      