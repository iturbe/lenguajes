-- Alonso Iturbe
-- A01021621

-- Combinaciones de x,y,z que cumplan con z^2=x^2+y^z
let res = [(x, y, z) | x <- [1..100], y <- [1..100], z <- [1..100], z^2 == x^2 + y^2]

-- Cuantos #s negativos hay entre -1000 y 1000
let int = [-1000..1000]
let res = [x | x <- int, x < 0]
length(res)

-- Suma de todos los números impares abajo de 1,000,000
let res = [x | b <- [1..1000000], b mod 2 != 0]
sum(res)

-- Convertir un string a toUpper usando list comprehension
import Data.Char
-- let string = "this is my message"
-- msg es lo que quieres convertir a uppercase
-- invocar con <res "hola">
res msg = [toUpper(y) | y <- msg]

-- Calcular el factorial de un número recursivamente
let factorial 0  = 1; factorial n  = n * factorial(n-1)

-- Devolver los n últimos números de una lista
-- n..temp es el intervalo de elementos que en realidad queremos
-- sacar los primeros ya sea poppeando o algo similar
let numeros = [1..100]
devuelve n = [x | let temp = length(numeros) - n, x <- ]