sucesor :: Int -> Int
sucesor n = n + 1

add :: Int -> Int -> Int
add a 0 = a
add a b = add (sucesor a) (b - 1)

multiplicar :: Int -> Int -> Int
multiplicar _ 0 = 0
multiplicar a b = add a (multiplicar a (b - 1))

predecesor :: Int -> Int
predecesor n = n - 1

restar :: Int -> Int -> Int
restar a 0 = a
restar a b = restar (predecesor a) (b - 1)

dividir :: Int -> Int -> Int
dividir a b
    | a < b     = 0
    | otherwise = 1 + dividir (restar a b) b

addReal :: Float -> Float -> Float
addReal x y = x + y

sucesorReal :: Float -> Float
sucesorReal x = x + 1.0

main :: IO ()
main = do
    putStr "pruebas de sucesor y predecesor: "
    print (sucesor 5)
    print (predecesor 5)

    putStr "pruebas de suma: "
    print (add 3 4)

    putStr "pruebas de multiplicacion: "
    print (multiplicar 3 4)

    putStr "pruebas de resta: "
    print (restar 10 3)

    putStr "pruebas de division: "
    print (dividir 12 3)
    print (dividir 14 3)

    putStr "pruebas con numeros reales: "
    print (addReal 2.5 3.7)
    print (sucesorReal 2.5)
