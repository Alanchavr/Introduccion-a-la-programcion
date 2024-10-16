import Test.HUnit
import Parcial 

-- Caso de prueba 1: Presidente con la mayoría de votos
test1 :: Test
test1 = TestCase (assertEqual "Caso 1: Presidente con más votos" 
                            "Cristina" 
                            (proximoPresidente [("Cristina", "Alberto"), ("Lula", "Geraldo"), ("Andrés", "Marcelo")] [600, 400, 200]))

-- Caso de prueba 2: Otro presidente con más votos
test2 :: Test
test2 = TestCase (assertEqual "Caso 2: Presidente con más votos" 
                            "Lula" 
                            (proximoPresidente [("Cristina", "Alberto"), ("Lula", "Geraldo"), ("Andrés", "Marcelo")] [300, 500, 200]))

-- Caso de prueba 3: Todos tienen los mismos votos
test3 :: Test
test3 = TestCase (assertEqual "Caso 3: Todos con los mismos votos"
                            "Cristina"
                            (proximoPresidente [("Cristina", "Alberto"), ("Lula", "Geraldo"), ("Andrés", "Marcelo")] [100, 100, 100]))

-- Caso de prueba 4: Sin presidentes
test4 :: Test
test4 = TestCase (assertEqual "Caso 4: Sin presidentes"
                            ""
                            (proximoPresidente [] []))

-- Caso de prueba 5: Solo un presidente
test5 :: Test
test5 = TestCase (assertEqual "Caso 5: Solo un presidente"
                            "Cristina"
                            (proximoPresidente [("Cristina", "Alberto")] [100]))

-- Agrupación de todos los tests
tests :: Test
tests = TestList [test1, test2, test3, test4, test5]

-- Ejecutar todos los tests
main :: IO Counts
main = runTestTT tests
