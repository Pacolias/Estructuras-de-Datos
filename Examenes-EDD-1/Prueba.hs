-- Programa de prueba de Haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial(n-1)