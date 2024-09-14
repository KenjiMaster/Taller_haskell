
-- ejercicio uno
factorial :: Int -> Int
factorial n
    | n == 1 = 1
    | otherwise = n * factorial (n - 1)
    
main :: IO()
main = do
    let a = 4
    putStrLn $ "El factorial es: "
    print(factorial a)


-- ejercicio 2
catalan n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = (2 * (2 * n - 1) / (n + 1)) * catalan(n - 1)
    
main :: IO()
main = do
    let n = 5
    putStrLn $ "El catalan es: "
    print(catalan n)


-- ejercicio 3
raizDigital :: Int -> Int
raizDigital n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (n `mod` 10) + raizDigital(n `div` 10)
    
main :: IO() 
main = do
    let n = 23
    putStrLn $ "La raizDigital es: "
    print(raizDigital n)


-- ejercicio 4
potenciaBase2 :: Int -> Int
potenciaBase2 n
    | n == 1 = 2
    | otherwise = potenciaBase2(n - 1) * 2
    
main :: IO()
main = do
    let a = 10
    putStrLn $ "La potenciaBase2 es: "
    print(potenciaBase2 a)


-- ejercicio 5
cuatroUltimos :: Int -> Int
cuatroUltimos n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = 1 + cuatroUltimos((n-1) `mod` 10000)
    
main :: IO()
main = do
    let a = 67267
    putStrLn $ "Los cuatroUltimos es: "
    print(cuatroUltimos a)


-- ejercicio 6
segundo n
    | n < 100 = n `mod` 10
    | otherwise = segundo (n `div` 10)
    
main :: IO()
main = do
    let a = 352345
    putStrLn $ "El segundo es: "
    print(segundo a)

-- ejercicio 7
tresUltimos n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = tresUltimos((n-1) `mod` 1000) + 1
    
main :: IO()
main = do
    let a = 352753
    putStrLn $ "Los tres ultimos son: "
    print(tresUltimos a)




-- ejercicio 8
primer n
    | n < 10 = n
    | otherwise = primer(n `div` 10)
    
main :: IO()
main = do
    let a = 937645
    putStrLn $ "El primero es: "
    print(primer a)
