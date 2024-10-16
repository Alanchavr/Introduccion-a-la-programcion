module Parcial  where




generarStock :: [String] -> [(String, Int)]
generarStock [] = []
generarStock (x:xs)  = (x, apariciones (x:xs) x): generarStock (quitarTodos x xs)

apariciones :: [String] -> String -> Int
apariciones [] _ = 0
apariciones (x:xs) s | x == s = 1 + apariciones xs s
                     | otherwise = apariciones xs s

quitarTodos:: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos s (x:xs) | x == s = quitarTodos s xs
                     | otherwise = x:quitarTodos s xs


-- Otra forma de hacerlo
--generarStock2 :: [String] -> [(String, Int)]
--generarStock2 [] = []
--generarStock2 (x:xs) = agregar x (generarStock xs)

--agregar :: String ->  [(String, Int)] ->  [(String, Int)]
--agregar p [] = [(p,1)]
--agregar p ((prod, cant):ps) | prod == p = (prod, cant+1):ps
  --                          | otherwise = (prod, cant):(agregar p ps)



--problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
--requiere: {True}
--asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas
--1-- ni tuplas con
--ambas componentes iguales}}
--1 A los fines de este problema consideraremos que dos tuplas son iguales si el par
--de elementos que las componen (sin importar el orden) son iguales.


relacionesValidas :: [(String, String)]->Bool
relacionesValidas []= True
relacionesValidas [x]= True
relacionesValidas (x:xs)= relacionesauxiliar (x:xs) && relacionescomponentes (x:xs) &&  relacionesValidas xs

relacionesauxiliar :: [(String, String)]->Bool
relacionesauxiliar []= True
relacionesauxiliar [x]= True
relacionesauxiliar  (x:y:xs) 
                            | x==y = False
                            |otherwise = relacionesauxiliar (x:xs)

relacionescomponentes ::  [(String, String)]->Bool
relacionescomponentes []= True
relacionescomponentes [x]=True
relacionescomponentes  ((x,y):xs)
                               | x==y = False
                               |otherwise = relacionescomponentes xs





--1 A los fines de este problema consideraremos que dos tuplas son iguales si el par
--de elementos que las componen (sin importar el orden) son iguales.
--problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--requiere: {relacionesValidas(relaciones)}
--asegura: {res no tiene elementos repetidos}
--asegura: {res tiene exactamente los elementos que figuran en alguna tupla de
--relaciones, en cualquiera de sus posiciones}
--}

personas :: [(String, String)]->[String]
personas  []=[]
personas (x:xs) =  quitarrepetidos (retornarpersona1 xs ++ retornarpersona2 xs)
        


retornarpersona1 :: [(String, String)]->[String] 
retornarpersona1  []=[]
retornarpersona1  ((x,y):xs)= x:retornarpersona1 (relacionespersonasvalidas xs)

retornarpersona2 :: [(String, String)]->[String] 
retornarpersona2  []=[]
retornarpersona2  ((x,y):xs)= y:retornarpersona2 (relacionespersonasvalidas xs)

relacionespersonasvalidas :: [(String, String)]->[(String, String)]
relacionespersonasvalidas  []=[]
relacionespersonasvalidas xs 
                            | relacionesValidas xs =xs



quitarrepetidos :: [String]->[String]
quitarrepetidos [] = []
quitarrepetidos (x:xs) 
               | pertenece x xs = quitarrepetidos xs 
               | otherwise = x: quitarrepetidos xs 

pertenece :: String->[String]-> Bool
pertenece _[]= False
pertenece s (x:xs) 
               |s ==x = True
               |otherwise = pertenece s xs



--problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--requiere: {relacionesValidas(relaciones)}
--asegura: {res tiene exactamente los elementos que figuran en las tuplas de
--relaciones en las que una de sus componentes es persona}
--}

--tengo que hacer una funcion que dado un nombre me de el nombres de las personas con las que es amigos, osea
--osea con la que esta relacionado el string, para eso puedo hacer un que dado un sting me devuele un lista de tuplas
--le doy un elemento y si es el nombre esta en la tupla me devuleva no la tupla sino el elemento con el que esta asi con todoas las tuplas

--amigosde :: String->[(String, String)]->[String]
--amigosde _[] =[]
--amigosd 


amigosde::String->[(String, String)]->[String]
amigosde _[] =[]
amigosde p ((x,y):xs) 
                     | p==x   = y:amigosde p xs
                     | p==y   = x:amigosde p xs
                     | otherwise = amigosde p xs


--problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
--requiere: {relaciones no vacía}
--requiere: {relacionesValidas(relaciones)}
--asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o
--alguno de ellos si hay empate)}

--personaConMasAmigos :: [(String, String)]-> String
--personaConMasAmigos []= ""
--personaConMasAmigo

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos xs =  buscarpersonasconmasamigos xs (personas xs)

cuentalosamigueros :: String->[(String, String)]-> Int
cuentalosamigueros p xs = longitudamiguera (amigosde p xs)

longitudamiguera :: [String]-> Int
longitudamiguera []=0
longitudamiguera (_:xs)= 1+ longitudamiguera xs

buscarpersonasconmasamigos :: [(String,String)]->[String]->String
buscarpersonasconmasamigos _[a]= a
buscarpersonasconmasamigos xs (a:b:ps)
                              | cuentalosamigueros a  xs >= cuentalosamigueros b xs = buscarpersonasconmasamigos xs (a:ps)
                              | otherwise = buscarpersonasconmasamigos xs (b:ps)


{-Explicación:
personaConMasAmigos: Llama a personas para obtener una lista de todas las personas en las relaciones y luego usa buscarPersonaConMasAmigos para encontrar a la persona con más amigos.

cuentalosamigueros: Devuelve la cantidad de amigos de una persona, utilizando la función amigosde que devuelve todos los amigos de esa persona.

buscarPersonaConMasAmigos: Compara recursivamente las personas en la lista para encontrar la que tiene más amigos.

Este código resuelve el problema sin agregar funciones externas y utilizando las que ya has definido.


--puedo usar amigos de para detemrinar la  cantidad de realciones de una persona en base a sumi luego suma los eleeos de esa peroa que
--sea la qe tengo que todos invitada, haz funiocn que cuente la cantidad de coelementos, que cuente los amigueros de carlito s en la lista de relaciones 

-}




--1. Viva la democracia
--La elección periódica de los gobernantes es la base de los Estados Modernos. Este sistema, denominado ”democracia” (término
--proveniente de la antigua Grecia), tiene diferentes variaciones, que incluyen diferentes formas de elección del/a máximo/a
--mandatario/a. Por ejemplo, en algunos países se eligen representantes en un colegio electoral (EEUU). En otros se vota a
--los/as miembros del parlamento (España). En nuestro país elegimos de forma directa la fórmula presidencial (Presidente/a y
--Vicepresidente/a) cada 4 años.
--A continuación presentamos una serie de ejercicios que tienen como objetivo implementar funciones para sistema de escrutinio
--de una elección presidencial. Leer las descripciones y especificaciones e implementar las funciones requeridas en Haskell, utilizado
--solamente las herramientas vistas en clase.
--Las fórmulas presidenciales serán representadas por tuplas (String x String), donde la primera componente será el nombre del
--candidato a presidente, y la segunda componente será el nombre del candidato a vicepresidente.
--En los problemas en los cuales se reciban como parámetro secuencias de fórmulas y votos, cada posición de la lista votos
--representará la cantidad de votos obtenidos por la fórmula del parámetro formulas en esa misma posición. Por ejemplo, si la
--lista de fórmulas es [(”Juan Pérez”,”Susana García”), (”María Montero”,”Pablo Moreno”)] y la lista de votos fuera [34, 56], eso
--indicaría que la fórmula encabezada por María Montero obtuvo 56 votos, y la lista encabezada por Juan Pérez obtuvo 34 votos.


--1.1. Ejercicio 1 - Votos en Blanco
--problema votosEnBlanco(formulas : seq < String × String >, votos : seq < Z >, cantTotalVotos : Z) : Z{
--requiere : {formulas Validas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
--asegura : {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron }
--}

--1.2. Ejercicio 2 - Fórmulas Válidas
--problema formulasValidas(formulas : seq < String × String >) : Bool{
--requiere : {True}
--asegura : {(res = true) ↔ formulas no contiene nombres repetidos, es decir que cada candidato está en una única fórmula (no se puede ser 
--candidato a presidente y a vicepresidente ni en la misma fórmula ni en fórmulas distintas) }
--}

--1.3. Ejercicio 3 - Porcentaje de Votos
--problema porcentajeDeVotos(presidente : String, formulas : seq < String × String >, votos : seq < Z >) : R{
--requiere : {La primera componente de algun elemento de formulas es presidente}
--requiere : {formulasValidas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {Hay al menos un elemento de votos que es mayor estricto que 0}
--asegura : {res es el porcentaje de votos que obtuvo la fórmula encabezada por presidente sobre el total de votos afirmativos}
--}
--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos números de tipo
--Int:
--division :: Int → Int → Float
--division a b = (fromIntegral a) / (fromIntegral b)

--1.4. Ejercicio 4 - Próximo Presidente
--problema proximoPresidente(formulas : seq < String × String >, votos : seq < Z >) : String{
--requiere : {La primera componente de algun elemento de formulas es presidente}
--requiere : {formulasValidas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {Hay al menos un elemento de votos que es mayor estricto que 0}
--requiere : {|formulas| > 0}
--asegura : {res es el candidato a presidente de formulas más votado de acuerdo a los votos contabilizados en votos}
--}






--1.1. Ejercicio 1 - Votos en Blanco
--problema votosEnBlanco(formulas : seq < String × String >, votos : seq < Z >, cantTotalVotos : Z) : Z{
--requiere : {formulas Validas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
--asegura : {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron }
--}





--1.2. Ejercicio 2 - Fórmulas Válidas
--problema formulasValidas(formulas : seq < String × String >) : Bool{
--requiere : {True}
--asegura : {(res = true) ↔ formulas no contiene nombres repetidos, es decir que cada candidato está en una única fórmula (no se puede ser 
--candidato a presidente y a vicepresidente ni en la misma fórmula ni en fórmulas distintas) }
--}


formulasValidas :: Eq a => [(a, a)] -> Bool
formulasValidas []=True
formulasValidas (x:xs) = formulasValidas1 x &&  formularepetida x xs  && formulaenotralista x xs && formulasValidas xs


formulaenotralista :: Eq a => (a, a)->[(a, a)] -> Bool
formulaenotralista _[]= True
formulaenotralista (x,v) ((y,z):ys) 
                    |x == y || x==z ||v ==y ||v==z   = False
                    |otherwise = formulaenotralista (x,v) ys

formularepetida :: Eq a => (a, a)->[(a, a)] -> Bool
formularepetida _[]= True
formularepetida x (y:ys) 
                    |x == y  = False
                    |otherwise = formularepetida x ys


formulasValidas1:: Eq a => (a, a)-> Bool
formulasValidas1  (x,y)
                    |x==y = False
                    |otherwise = True

--1.1. Ejercicio 1 - Votos en Blanco
--problema votosEnBlanco(formulas : seq < String × String >, votos : seq < Z >, cantTotalVotos : Z) : Z{
--requiere : {formulas Validas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
--asegura : {res es la cantidad de votos emitidos que no correspondieron a niguna de las fórmulas que se presentaron }
--}



votosEnBlanco :: [(String,String)]->[Integer]-> Integer-> Integer
votosEnBlanco [][] z = z
votosEnBlanco xs y z =  z - sumadevotos y

sumadevotos :: [Integer]-> Integer
sumadevotos [] =0
sumadevotos (x:xs) = x + sumadevotos xs


--1.3. Ejercicio 3 - Porcentaje de Votos
--problema porcentajeDeVotos(presidente : String, formulas : seq < String × String >, votos : seq < Z >) : R{
--requiere : {La primera componente de algun elemento de formulas es presidente}
--requiere : {formulasValidas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {Hay al menos un elemento de votos que es mayor estricto que 0}
--asegura : {res es el porcentaje de votos que obtuvo la fórmula encabezada por presidente sobre el total de votos afirmativos}
--}
--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos números de tipo
--Int:
--division :: Int → Int → Float
--division a b = (fromIntegral a) / (fromIntegral b)



porcentajeDeVotos :: String->[(String,String)]->[Integer]->Float
porcentajeDeVotos x ys zs = (division (formulaysusvotos  x ys zs) (sumadevotos zs))*100


formulaysusvotos :: String->[(String,String)]->[Integer]->Integer
formulaysusvotos  _ [] []= 0
formulaysusvotos  x ((v,w):ys) (t:zs)
                           | x==v = t
                           |otherwise = formulaysusvotos  x ys zs
                          


division :: Integer-> Integer ->Float
division a b = (fromIntegral a ) / (fromIntegral b)
 
 
 --dividir el elemento de que le correspondea la formula sobre el total de los votos



--1.4. Ejercicio 4 - Próximo Presidente
--problema proximoPresidente(formulas : seq < String × String >, votos : seq < Z >) : String{
--requiere : {La primera componente de algun elemento de formulas es presidente}
--requiere : {formulasValidas(formulas)}
--requiere : {|formulas| = |votos|}
--requiere : {Todos los elementos de votos son mayores o iguales que 0}
--requiere : {Hay al menos un elemento de votos que es mayor estricto que 0}
--requiere : {|formulas| > 0}
--asegura : {res es el candidato a presidente de formulas más votado de acuerdo a los votos contabilizados en votos}
--}


proximoPresidente :: [(String,String)]->[Integer]->String
proximoPresidente []_ = ""
proximoPresidente xs ys = compararpresidentesysusvotos(presidenteysusvotos xs ys)

compararpresidentesysusvotos :: [(String,Integer)]->String
compararpresidentesysusvotos [] = ""
compararpresidentesysusvotos [(v,w)] = v
compararpresidentesysusvotos ((v,w):(z,y):xs)
                               |w>=y = compararpresidentesysusvotos ((v,w):xs)
                               |otherwise = compararpresidentesysusvotos ((z,y):xs)

presidenteysusvotos :: [(String,String)]->[Integer]->[(String,Integer)]
presidenteysusvotos  [] _ = []
presidenteysusvotos  ((v,w):ys) (t:zs) = (v,t): presidenteysusvotos ys zs



{-
Fila = seq⟨Z⟩
Tablero = seq⟨F ila⟩
Posicion = Z × Z – Observaci´on: las posiciones son: (fila, columna)
Camino = seq⟨P osicion⟩
Ejercicio 5. Implementar la funcion maximo :: Tablero ->Int
problema maximo (t: Tablero) : Z {
requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
menos un elemento}
requiere: {Existe al menos una columna en el tablero t }
requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
asegura: {res es igual al n´umero m´as grande del tablero t
-}
{-}
type Fila =[Int]
type Tablero = [Fila]
type Posicion = (Int,Int)
type Camino = [Posicion]


maximo :: Tablero ->Int
maximo tablero = maximoFila (concatenarMaximos tablero)    

--Funcin que busca  el  maximo  de  una lista  de  enteros
maximoFila :: Fila -> Int
maximoFila [x] = x
maximoFila (x:y:xs) | x >= y = maximoFila (x:xs)
                    | x < y = maximoFila (y:xs)      

--Funcion que va  concatenando los  maximos  de  una lista de  lista  de  enteros

concatenarMaximos ::  Tablero -> Fila
concatenarMaximos []  = []
concatenarMaximos (fila:tablero) = maximoFila fila : concatenarMaximos tablero   
-}

{- 
Ejercicio 6. Implementar la funci´on masRepetido :: Tablero ->Int
problema masRepetido (t: Tablero) : Z {
requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
menos un elemento}
requiere: {Existe al menos una columna en el tablero t }
requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
asegura: {res es igual al n´umero que m´as veces aparece en un tablero t. Si hay empate devuelve cualquiera de ellos}
}

-}


{-}
masRepetido :: Tablero -> Int
masRepetido [f] = masRepetidoInt f f
masRepetido (f1:f2:xs) 
  | cantAparicionesInt f1 (masRepetidoInt f1 f1) > cantAparicionesInt f2 (masRepetidoInt f2 f2) = masRepetido (f1:xs)
  | otherwise = masRepetido (f2:xs)

masRepetidoInt :: Fila -> Fila -> Int
masRepetidoInt [a] ys = a
masRepetidoInt (x:y:xs) ys
  | cantAparicionesInt ys x > cantAparicionesInt ys y = masRepetidoInt (x:xs) ys --devuelve el numero mas repetido comparando una fila con otra
  | otherwise = masRepetidoInt (y:xs) ys

cantAparicionesInt :: Fila -> Int -> Int   ------devuelve la cantidad de apariciones o repeticiones
cantAparicionesInt [] _ = 0
cantAparicionesInt (x:xs) y 
  | y == x = 1 + cantAparicionesInt xs y
  | otherwise = cantAparicionesInt xs y
-}

{- }
Ejercicio 7. Implementar la funci´on valoresDeCamino :: Tablero ->Camino ->[Int]
problema valoresDeCamino (t: Tablero, c: Camino) : seq⟨Z⟩ {
requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
menos un elemento}
requiere: {Existe al menos una columna en el tablero t }
requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
requiere: {El camino c es un camino v´alido, es decir, secuencia de posiciones adyacentes en la que solo es posible
desplazarse hacia la posici´on de la derecha o hacia abajo y todas las posiciones est´an dentro de los limites del tablero
t}
asegura: {res es igual a la secuencia de n´umeros que est´an en el camino c, ordenados de la misma forma que aparecen
las posiciones correspondientes en el camino.}
}
-}


--puedo hacer un lista de tupla que tenga el numero y su poscion en base a cada posiscion otcden una lista en oreden aparecin de la lista
{-

-- Función que devuelve el valor de una lista en una posición sin usar !!
elementoEnIndice :: [a] -> Int -> a
elementoEnIndice (x:xs) 0 = x
elementoEnIndice (x:xs) n = elementoEnIndice xs (n-1)

-- Función que devuelve el valor en una posición específica del tablero
valorDePosicion :: Tablero -> Posicion -> Int
valorDePosicion tablero (fila, columna) = elementoEnIndice (elementoEnIndice tablero fila) columna

-- Función principal que devuelve los valores del camino dado
valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino tablero [] = []
valoresDeCamino tablero (p:ps) = valorDePosicion tablero p : valoresDeCamino tablero ps

-}

{- }
-- Función auxiliar para obtener el valor en una posición específica del tablero
valorEnPosicion :: Tablero -> (Int, Int) -> Int
valorEnPosicion tablero (x, y) = obtenerFila (obtenerFila tablero x) y
  where
    obtenerFila (f:fs) 0 = f
    obtenerFila (_:fs) n = obtenerFila fs (n-1)
    obtenerFila [] _ = error "Índice fuera de rango"

-- Función principal que recorre el camino y obtiene los valores
valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = []
valoresDeCamino tablero ((x, y):cs) = valorEnPosicion tablero (x, y) : valoresDeCamino tablero cs

-}
{-
Perfectos amigos
El Departamento de Matem´atica (DM) de la FCEyN-UBA nos ha encargado que desarrollemos un sistema para el
tratamiento de n´umeros naturales. Espec´ıficamente les interesa conocer cu´ando un n´umero es perfecto y cu´ando dos n´umeros
son amigos. Aunque por ah´ı no lo sab´ıas, estos conceptos existen y se definen como:
Un n´umero natural es perfecto cuando la suma de sus divisores propios (n´umeros que lo dividen menores a ´el) es igual
al mismo n´umero. Por ejemplo, 6 es un n´umero perfecto porque la suma de sus divisores propios (1,2 y 3) es igual a 6.
Dos n´umeros naturales distintos son amigos si cada uno de ellos se obtiene sumando los divisores propios del otro.
Por ejemplo, 220 y 284 son amigos porque los divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y 110 que
sumados dan 284 y los divisores propios de 284 son 1, 2 , 4, 71, 142 que sumados dan 220.
Para implementar este sistema nos enviaron las siguientes especificaciones en lenguaje semiformal y nos pidieron que hagamos
el desarrollo enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que se ven en la materia
Introducci´on a la Programaci´on / Algoritmos y Estructuras de Datos I (FCEyN-UBA).

-}

{-
Implementar la funci´on divisoresPropios :: Int ->[Int]
problema divisoresPropios (n: Z) : seq⟨Z⟩ {
requiere: {n > 0}
asegura: {res es la lista de divisores propios de n, ordenada de menor a mayor}
}

-}

divisoresPropios :: Int ->[Int]
divisoresPropios n = obtenerdivisores n 1

obtenerdivisores :: Int -> Int -> [Int]
obtenerdivisores n b
    | b >= n         = []
    | n `mod` b == 0 = b : obtenerdivisores n (b + 1)
    | otherwise      = obtenerdivisores n (b + 1)

{- 
Ejercicio 10. Implementar la funci´on sonAmigos :: Int ->Int ->Bool
problema sonAmigos (n,m: Z) : Bool {
requiere: {n > 0}
requiere: {m > 0}
requiere: {m ̸= n}
asegura: {res = True ⇔ n y m son n´umeros amigos}
}
Dos n´umeros naturales distintos son amigos si cada uno de ellos se obtiene sumando los divisores propios del otro.
Por ejemplo, 220 y 284 son amigos porque los divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y 110 que
sumados dan 284 y los divisores propios de 284 son 1, 2 , 4, 71, 142 que sumados dan 220.

-}


sonAmigos :: Int->Int->Bool
sonAmigos a b 
             |sumasdeenteros (divisoresPropios a) ==b || sumasdeenteros (divisoresPropios b) ==a  =True
             |otherwise= False



sumasdeenteros :: [Int]-> Int 
sumasdeenteros []= 0
sumasdeenteros (x:xs) = x + sumasdeenteros xs

{-
Ejercicio 11. Implementar la funci´on losPrimerosNPerfectos :: Int ->[Int]
problema losPrimerosNPerfectos (n: Z) : seq⟨Z⟩ {
requiere: {n > 0}
asegura: {res es la lista de los primeros n n´umeros perfectos, de menor a mayor}
}
Por cuestiones de tiempos de ejecuci´on, no les recomendamos que prueben este ejercicio con un n > 4.

-}

{-Un n´umero natural es perfecto cuando la suma de sus divisores propios (n´umeros que lo dividen menores a ´el) es igual
al mismo n´umero. Por ejemplo, 6 es un n´umero perfecto porque la suma de sus divisores propios (1,2 y 3) es igual a 6
-}


losPrimerosNPerfectos :: Int ->[Int]
losPrimerosNPerfectos a = inversoLista (nPerfectos a)


inversoLista :: [Int] -> [Int]
inversoLista [] = []
inversoLista (x:xs) = inversoLista xs ++ [x]


nPerfectos :: Int ->[Int]
nPerfectos 0 =[]
nPerfectos a
             |sumasdeenteros(divisoresPropios a) == a  =a: nPerfectos(a-1)
             |otherwise= nPerfectos (a-1)


{- 
Ejercicio 12. Implementar la funci´on listaDeAmigos :: [Int] ->[(Int,Int)]
problema sonAmigos (lista: seq⟨Z⟩) : seq⟨Z × Z⟩ {
requiere: {Todos los n´umeros de lista son mayores a 0}
requiere: {Todos los n´umeros de lista son distintos}
asegura: {res es una lista de tuplas sin repetidos, que contienetuplas de dos n´umeros donde esos dos n´umeros
pertenecen a lista y son amigos}
asegura: {|res| es la cantidad de tuplas de dos n´umeros amigos que hay en lista. Consideraremos que la tupla (a, b)
(con a y b pertenecientes a Z) es igual a la tupla (b, a) para contar la cantidad de tuplas.}
}

-}


listaDeAmigos :: [Int] ->[(Int,Int)]
listaDeAmigos []=[]
listaDeAmigos (x:xs)= buscoamigos x xs ++ listaDeAmigos xs
    

buscoamigos :: Int ->[Int] ->[(Int,Int)]
buscoamigos _ []= []
buscoamigos x (y:xs)
      |sonAmigos x y = (x,y): buscoamigos x xs
      |otherwise = buscoamigos x xs





--VAMOS CAMPEÓN!
--En Exactas se está jugando un torneo de futbol y la facultad le pidió a los alumnos de IP programar algunas funcionalidades en Haskell.
--Los datos con los que contamos para esto son los nombres de los equipos que participan del torneo, los nombres de los goleadores de cada
--uno de dichos equipo, y la cantidad de goles convertidos por esos jugadores. Los nombres de los equipos y sus respectivos goleadores serán
--modelados mediante tuplas de tipo (String,String), donde la primera componente representa el nombre del equipo, y la segunda representa el
--nombre del goleador de dicho equipo.

--En los problemas en los cuales se reciban, como parámetros, secuencias _goleadoresPorEquipo_ y _goles_, cada posicion de la lista goles representará
--la cantidad de goles obtenidos por el goleador del equipo que se encuentra en esa misma posición de _goleadoresPorEquipo_.
--Por ejemplo si la lista goleadoresPorEquipo es [("Sacachispas","Robertino Giacomini"),("Fénix","Matias Dominguez")] y la lista goles es [3,5], eso indica
--que Robertino Giacomini metió 3 goles y Matias Dominguez metió 5.

----------------------------------------------------------------------------------------------------------------------------------------------------------

--1) Goles de no goleadores [1 punto]

--problema golesDeNoGoleadores (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩, totalGolesTorneo: Z ): Z {
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
--    asegura: {res es la cantidad de goles convertidos en el torneo por jugadores que no son los goleadores de sus equipos}

--}






-----------------------------------------------------------------------------------------------------------------------------------------------------------

--2) Equipos Válidos [3 puntos]

--problema equiposValidos (goleadoresPorEquipo: seq⟨String x String⟩): Bool{
--    requiere: {True}
--    asegura: {(res = True) <-> goleadoresPorEquipo no contiene nombres de clubes repetidos, ni goleadores repetidos, ni jugadores con nombre de club}
--}

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--3) Porcentaje de Goles [3 puntos]

--problema porcentajeDeGoles (goleador: String, goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): R {
--    requiere: {La segunda componente de algún elemento de goleadoresPorEquipo es goleador}
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {Hay al menos un elemento de goles mayor estricto a 0}
--    asegura: {res es el porcentaje de goles que marcó goleador sobre el total de goles convertidos por goleadores}
--}

--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos numeros de tipo Int:
--division :: Int -> Int -> Float
--division a b = (fromIntegral a) / (fromIntegral b)

-----------------------------------------------------------------------------------------------------------------------------------------------------------

--4) Botín de Oro [3 puntos]

--problema botinDeOro (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): String {
--    requiere: {equiposValidos(goleadoresPorEquipo)}
--    requiere: {|goleadoresPorEquipo| = |goles|}
--    requiere: {Todos los elementos de goles son mayores o iguales a 0}
--    requiere: {|goles| > 0}
--    asegura: {res es alguno de los goleadores de goleadoresPorEquipo que más tantos convirtió de acuerdo a goles}
--}


golesDeNoGoleadores :: [(String, String)] -> [Int] -> Int -> Int
golesDeNoGoleadores _ [] z = z
golesDeNoGoleadores _ xs z = z - sumadegoles xs


sumadegoles ::[Int]->Int
sumadegoles []=0
sumadegoles (x:xs)= x + sumadegoles xs




aplanar :: [(String, String)] -> [String]
aplanar [] = []
aplanar ((a,b):xs) = a : b : aplanar xs

pertenece :: String -> [String] -> Bool
pertenece x [] = False 
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys   

hayRepetidos :: [String] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

equiposValidos :: [(String, String)] -> Bool
equiposValidos xs = not (hayRepetidos(aplanar xs)




listainversa :: [Integer]->[Integer]
listainversa [] = []
listainversa (x:xs) = listainversa xs ++ [x]

primerosNelementos :: [Integer]->Integer->[Integer]
primerosNelementos _ 0 = []
primerosNelementos [] _ = []
primerosNelementos  (x:xs) n = x: primerosNelementos xs (n-1)

sumaelementos :: [Integer]->Integer
sumaelementos []=0
sumaelementos (x:xs) =  x + sumaelementos xs