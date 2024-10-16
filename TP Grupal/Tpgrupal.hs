type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Float)
type AgenciadeViajes = [Vuelo]

vuelosvalidos :: AgenciadeViajes -> Bool
vuelosvalidos [] = True
vuelosvalidos xs = vuelosvalidosaux xs && vuelovalidoindividuales xs

vuelosvalidosaux :: AgenciadeViajes -> Bool
vuelosvalidosaux [] = True
vuelosvalidosaux (x:xs) = vuelosrepetidosyvalidos x xs && vuelosvalidosaux xs

vuelosrepetidosyvalidos :: Vuelo -> AgenciadeViajes -> Bool
vuelosrepetidosyvalidos _ [] = True
vuelosrepetidosyvalidos (x, y, z) ((v, w, j):ys)
    | (x == v && y == w) = False
    | otherwise = vuelosrepetidosyvalidos (x, y, z) ys

vuelovalidoindividuales :: AgenciadeViajes -> Bool
vuelovalidoindividuales [] = True
vuelovalidoindividuales (x:xs)
    | vuelovalido x = vuelovalidoindividuales xs
    | otherwise = False

vuelovalido :: Vuelo -> Bool
vuelovalido (x, y, z)
    | x == y || z <= 0 = False
    | otherwise = True

-- Ejercicio 2
ciudadesconectadas :: AgenciadeViajes -> Ciudad -> [Ciudad]
ciudadesconectadas [] _ = []
ciudadesconectadas (x:xs) j = eliminarDuplicados (agregarSiConectada (ciudadconectada x j) (ciudadesconectadas  xs j))

agregarSiConectada :: Ciudad -> [Ciudad] -> [Ciudad]
agregarSiConectada "" xs = xs
agregarSiConectada ciudad xs = ciudad : xs

ciudadconectada :: Vuelo -> Ciudad -> Ciudad
ciudadconectada (x, y, _) j
    | x == j = y
    | y == j = x
    | otherwise = ""

eliminarDuplicados :: [Ciudad] -> [Ciudad]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs)
    | estaEnLista x xs = eliminarDuplicados xs
    | otherwise = x : eliminarDuplicados xs

estaEnLista :: Ciudad -> [Ciudad] -> Bool
estaEnLista _ [] = False
estaEnLista y (z:zs)
    | y == z = True
    | otherwise = estaEnLista y zs

-- Ejercicio 3
modernizarFlota :: AgenciadeViajes -> AgenciadeViajes
modernizarFlota xs
    | vuelosvalidos xs = modernizarFlotaaux xs
    | otherwise = []

modernizarFlotaaux :: AgenciadeViajes -> AgenciadeViajes
modernizarFlotaaux [] = []
modernizarFlotaaux ((x, y, z):xs) = (x, y, z * 0.9) : modernizarFlotaaux xs



ciudadMasConectada :: AgenciadeViajes -> Ciudad
ciudadMasConectada [] = ""
ciudadMasConectada xs = ciudadConMasConexiones (calcularConexiones xs)

calcularConexiones :: AgenciadeViajes -> [(Ciudad, Int)]
calcularConexiones [] = []
calcularConexiones ((origen, destino, _):xs) = cantidadconexiones origen (cantidadconexiones destino (calcularConexiones xs))

cantidadconexiones :: Ciudad -> [(Ciudad, Int)] -> [(Ciudad, Int)]
cantidadconexiones ciudad [] = [(ciudad, 1)]
cantidadconexiones ciudad ((c, n):cs)
    | ciudad == c = (c, n + 1) : cs
    | otherwise = (c, n) : cantidadconexiones ciudad cs

ciudadConMasConexiones :: [(Ciudad, Int)] -> Ciudad
ciudadConMasConexiones [] = ""
ciudadConMasConexiones [(ciudad, _)] = ciudad
ciudadConMasConexiones ((ciudad1, n1):(ciudad2, n2):xs)
    | n1 >= n2 = ciudadConMasConexiones ((ciudad1, n1):xs)
    | otherwise = ciudadConMasConexiones ((ciudad2, n2):xs)


-- Ejercicio 5


sePuedeLlegar :: AgenciadeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False
sePuedeLlegar xs origen destino = sellegasoloconunaescala xs origen destino || sellegadesdecuidad xs origen destino


sellegasoloconunaescala :: AgenciadeViajes -> Ciudad -> Ciudad -> Bool
sellegasoloconunaescala [] _ _ = False
sellegasoloconunaescala [_] _ _ = False
sellegasoloconunaescala ((x,y,z):(v,w,_):xs) origen destino
    | origen == x && y == v && w == destino = True
    | otherwise = sellegasoloconunaescala ((x,y,z):xs) origen destino


sellegadesdecuidad :: AgenciadeViajes -> Ciudad -> Ciudad -> Bool
sellegadesdecuidad [] _ _ = False
sellegadesdecuidad ((x,y,z):xs) origen destino
    | x == origen && y == destino = True
    | otherwise = sellegadesdecuidad xs origen destino


-- Ejercicio 6
--duracionDelCaminoMasRapido  :: AgenciadeViajes -> Ciudad-> Ciudad -> Duracion




                                 





-- Ejercicio 7

puedoVolverAOrigen :: AgenciadeViajes -> Ciudad -> Bool
puedoVolverAOrigen agencia origen = || vueloConEscalas agencia origen origen

-- Función que verifica si existe un vuelo directo de ida y vuelta

vuelosdirectos :: AgenciadeViajes -> Ciudad -> Bool
vuelosdirectos [] _ _ = False 
vuelosdirectos ((x, y, _):xs) origen 
                                | x== origen = 

