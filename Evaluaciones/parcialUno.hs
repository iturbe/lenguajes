-- Alonso Iturbe
-- A01021621

data Tree a = EmptyTree | Node a (Tree a)(Tree a) deriving (Show, Read, Eq)

main = do

  {- 1
  2520 es el entero positivo más pequeño que puede ser dividido por cualquier de los números entre 1 y 10 sin que exista un residuo. Encuentra el entero positivo más pequeño que puede ser dividido por cualquiera de los números entre 1 y 20 sin que exista residuo (residuo cero)
  -}

  -- Naive approach (se tarda mucho, cómo se puede comprimir esto?)
  -- let entero = [ x | x <- [1..], x `mod` 2 == 0, x `mod` 3 == 0, x `mod` 4 == 0, x `mod` 5 == 0, x `mod` 6 == 0, x `mod` 7 == 0, x `mod` 8 == 0, x `mod` 9 == 0, x `mod` 10 == 0, x `mod` 11 == 0, x `mod` 12 == 0, x `mod` 13 == 0, x `mod` 14 == 0, x `mod` 15 == 0, x `mod` 16 == 0, x `mod` 17 == 0, x `mod` 18 == 0, x `mod` 19 == 0, x `mod` 20 == 0]

  -- esta sintáxis no sirve
  -- let numero = [ x | x <- [1..], y <- [1..20], mod x y == 0]
  -- print ( take 5 (numero) )

  -- usar map para mappear cada elemento de una given list a una función
  let list = [1..20]
  let testNumber n = sum ( map (mod n) list)
  let entero = [ x | x <- [1..], testNumber x == 0]

  -- print ( take 1 (entero) )

  {- 2
  Dada una secuencia de números primos, siendo el primer primo el número 2, el tercer primo el 3 y así sucesivamente,  cuál es el número primo 10,001 de la secuencia?
  -}

  let listPrimes n = null [ x | x <- [2..n-1], mod n x  == 0]
  print ( (filter listPrimes [2..])!!10000 ) -- utilizamos 10000 porque empezamos en índice 0

  {- 3
  Un número palíndromo es aquel que se lee igual de izquierda a derecha que de derecha a izquierda. El palíndromo más grande formado por el producto de dos enteros de dos dígitos es el número 9009 (formado a partir del producto de 91x99). Cuál es el palíndromo más grande formado a partir del producto de dos números de 3 dígitos?
  -}

  let palindrome = maximum[ a*b | a <- [100..999], b <- [100..999], reverse (show (a*b)) == show (a*b)]
  print ( palindrome )

  {- 4
  Realiza una función que reciba un árbol y que imprima los valores del árbol en INORDER
  -}

  -- Given
  let insertTree x EmptyTree = Node x (EmptyTree)(EmptyTree)
  let insertTree x (Node y left right)
        | x == y = Node x left right
        | x < y  = Node y (insertTree x left) right
        | x > y  = Node y left (insertTree x right)

  let printer EmptyTree = [""]
  let printer (Node a b c) =  (printer b)++[show a]++(printer c)

  let tree = Node 7 (Node 6 EmptyTree EmptyTree) (Node 9 (Node 8 EmptyTree EmptyTree) EmptyTree)
  print (printer tree)
