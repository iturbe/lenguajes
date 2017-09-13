{-
deriving (TypeClass)
  Show -> permite imprimir como String
  Read -> permite convertir de String al DataType
  Ord -> permite comparar
  Bounded -> permite obtener valores mínimo y máximo
-}

-- data ____ es usado para para definir un tipo de dato nuevo
-- Data Types
data Point = Point Float Float deriving (Show)

data Shape = Circle Float
            | Rectangle Point Point deriving (Show)

data Date = Date Int String Int deriving (Show)

-- igual que arriba, sólo que un poco más específico y flexible por el Read (permite generarlo a partir de un string)
data Fecha = Fecha{day::Int, month::String, year::Int} deriving (Show, Read)

data Pair key value = Pair key value deriving (Show)

data Lista l = Vacia | Concat l (Lista l) deriving (Show)

data Tree x = EmptyTree | Node x (Tree x) (Tree x) deriving (Show)

-- Functions
getDay(Date num _ _) = num
getMonth(Date _ month _) = month
getYear(Date _ _ year) = year

insertt x EmptyTree = Node x EmptyTree EmptyTree
insertt x (Node z le ri)
        | x == z    = Node z le ri
        | x < z     = Node z (insertt x le) ri
        | otherwise = Node z le (insertt x ri)

listToTree (f:l) = ft f l EmptyTree
              where ft val (h:t) currentTree
                    | t == []   = insertt h (insertt val currentTree)
                    | otherwise = ft h t (insertt val currentTree)

-- Para correr como módulo: quitar main = do, no puede haber lets, no puede haber prints
main :: IO ()
main = do
  {-
  let area (Circle r) = pi*r*r
  print (area (Circle 3.2))

  let p = (Point 3.2 5.6)

  let date = Date 5 "Sept" 2017
  print (date)
  print (getYear(date))

  let f = Fecha{year=2017, month="September", day=5}
  -- nota: para read(), necesitan estar en el mismo órden en el que se declararon!
  let x = read "Fecha{day=5, month=\"September\", year=2017}"::Fecha
  print (x)

  -- let z = Pair 5 "hola"

  -}

  let myTree = EmptyTree
  print (insertt 8 myTree)
  print (insertt 6 (insertt 7 (insertt 9 myTree)))

  print(listToTree([1, 2, 3, 4, 5]))
