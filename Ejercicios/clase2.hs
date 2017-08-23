main = do

-- Calcula un factorial recursivamente
let factorialTail x = ft(x-1) (x)
      where ft n res
                    | n == 0 = res
                    | otherwise = ft (n-1)(res*n)

print(factorialTail(5))

-- Recibe una lista como parámetro y la reversa
let invertList (h:t) = ft (t) ([h])
                        where ft (p:s) (t)
                                | s == [] = p:t
                                | otherwise = ft (s) (p:t)


print ( invertList( [1, 2, 3, 4, 5] ) )

-- Recibe una lista como parámetro y retorna una tupla con el máximo y el mínimo
let maxMinList x = mm (tail x) (head x) (head x)
                        where mm x maxx minn
                                | x == [] = (maxx, minn)
                                | (head x) > maxx = mm (tail x) (head x) minn
                                | (head x) < minn = mm (tail x) maxx (head x)
                                | otherwise = mm (tail x) maxx minn


print ( maxMinList( [1, 2, 3, 4, 5] ) )
