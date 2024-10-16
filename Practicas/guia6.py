## Ejercicio 1
import math
def  imprimir_hola_mundo () :
 
  print("¡Hola mundo!")
 


imprimir_hola_mundo()



def imprimir_un_verso():
  print("No Vuelvas\nsin razon \nNo vuelvas")


imprimir_un_verso()


def raizDe2()-> float:
  print(round(2 ** 0.5,4))
   

raizDe2()


def factorial_de_dos()-> int:
  print(math.factorial(2)) 

factorial_de_dos ()


def perimetro ()-> float:
  print(math.pi*2)
  
perimetro()

# Ejercicio 2

def imprimir_saludo(nombre:str):
 print("Hola "+nombre)

imprimir_saludo("lachi")

def raiz_cuadrada_de(numero:int)-> int:
  print(math.sqrt(numero))

raiz_cuadrada_de(9)

def  fahrenheit_a_celsius(fahrenheit:float)->float:
  print(((fahrenheit-32)*5)//9)


fahrenheit_a_celsius(450)

def imprimir_dos_veces(estribillo:str):
  print(estribillo*2)

imprimir_dos_veces("shizana koro ni wa")

def es_multiplo_de (n:int,m:int)->bool:
  return (n%m==0)

resultado = es_multiplo_de(10, 5)  
print(resultado) 

def es_par(numero:int)->bool:
  return(es_multiplo_de(numero,2))

resultado = es_par(10)  
print(resultado) 

def cantidad_de_pizzas(comenesales:int,min_cant_de_porciones:int)->int:
  return math.ceil((comenesales *min_cant_de_porciones)/8)

resultado = cantidad_de_pizzas(10,5) 
print(resultado) 

# Ejercicio 2

def alguno_es_0(numero1:float, numero2:float)-> bool:
  return (numero1==0) or (numero2==0)

def ambos_son_0(numero1:float, numero2:float) -> bool:
  return (numero1==0) and (numero2==0)


def  es_nombre_largo(nombre:str) -> bool:
  return (len(nombre)>= 3 and len(nombre) <= 8)

resultado = es_nombre_largo("alan")
print(resultado) 

def es_bisiesto(año:int)->bool:
  return (año%4==0) and ((año%100!=0) or (año%400==0))


def peso_pino(altura: float) -> float:
    peso_hasta_3_metros = min(altura, 3) * 100 * 3 
    peso_sobre_3_metros = max(altura - 3, 0) * 100 * 2  
    return peso_hasta_3_metros + peso_sobre_3_metros

def es_peso_util(peso_pino)->bool:
   return 400 <= peso_pino <= 1000
   
def  sirve_pino (altura):
 return es_peso_util(peso_pino(altura))
   
#ejercicio 5

def devolver_el_doble_si_es_par(numero:int)-> int:
  if numero%2==0:
    return (numero *2)
  else :
   return numero


def devolver_valor_si_es_par_sino_el_que_sigue(numero:int)-> int :
   if numero % 2 == 0 :
     return numero
   else :
    return numero +1

def  devolver_el_doble_si_es_multiplo3_el_triple_si_es_multiplo9(numero:int)-> int:
  if numero%3==0:
    return numero*2
  elif numero%9==0 :
   return numero*3

def lindo_nombre(nombre:str):
   if len(nombre) >= 5:
    print( "Tu nombre tiene muchas letras!")
   else:
    print("Tu nombre tiene menos de 5 caracteres")

def  elRango(numero:int):
  if numero<5:
    print("Menor a 5")
  elif  10<numero<20:
    print("Entre 10 y 20")
  else:
    print("mayor a 20")

def se_labura(sexo:chr,edad:int):
  if sexo=="F" and edad> 60 or sexo=="M" and edad>=65:
    print("Anda de vacaciones")
  elif edad<18:
    print("Anda de vacaciones")
  else :
    print("Anda a laburar")


#ejercicio 6 (implementar whhile)

def uno_a_diez():
  n=1
  while n<=10:
    print(n)
    n+=1
   

def pares ():
  n=10
  while (n<=40):
    print(n)
    n+=2

def eco():
  n=1
  while n<=10 :
    print("eco")
    n+=1

def despegue(numero:int)->str:
  while numero>0:
    print(numero)
    numero = numero - 1 
  print("despeguen")


def viaje_tiempo (anopartida:int,anollegada:int):
  anopartida-=0
  while (anopartida>=anollegada):
    print("Viajó un año al pasado, estamos en el",(anopartida))
    anopartida-=1


def viaje_tiempo_grecia (anopartida:int):
  anopartida-=0
  anollegada=-384
  while (anopartida>=anollegada):
    print("Viajó un año al pasado, estamos en el",(anopartida))
    anopartida-=20

#Ejercicio 7(implementar for i) hasta tres paramentres(start, stop, step)

def uno_a_diez() -> None:
    for n in range(1,11,1):
        print(n)

def pares ():
  for i in range(10,41,2):
     print(i)

def eco():
 for i in range(0,10,1):
    print("eco")

def cohete_V2(numero):
  for i in range(numero,0,-1):
    print(numero)
    numero-=1
  print("despeguen")

def viaje_tiempo_v2(anopartida:int,anollegada:int):
  for i in range(anopartida,anollegada,-1):
    print("Viajó un año al pasado, estamos en el",(anopartida))
    anopartida-=1

def viaje_tiempo_greciav2(anopartida:int):
  for i in range(anopartida,-384,-20):
    print("Viajó un año al pasado, estamos en el",(anopartida))
    anopartida-=20


#Ejercicio 8
x = 5
y = 7
x = x + y

x = 5
y = 7
z = x + y
y = z * 2   #nuevo valor para la variable y

x = 5
y = 7
x = "hora"
y = x * 2   #"horahora" en python, multiplicar un string por un número entero repite el string


x = False
res = not(x)   #not(False) = True


x = False
x = not(x)    # not(False) = True, Valor final de x: True

x = True
y = False
res = x and y
x = res and x   #x = False, y = False, res = False


#Ejercicio 9


def rt(x: int, g: int) -> int:
g = g + 1
return x + g

g: int = 0
def ro(x: int) -> int:
global g
g = g + 1
return x + g

#La función incrementa el valor de g global en 1 y luego retorna la 
# suma de x y el nuevo valor de g. El valor de g se mantiene entre 
# llamadas debido a que la función utiliza la palabra clave global 
# para modificar la variable fuera de su ámbito local.