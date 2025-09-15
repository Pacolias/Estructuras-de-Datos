module Main where

import SparseMatrix

main :: IO ()
main = do
    putStrLn "\n--- Test: fromList ---"
    let sm1 = fromList 3 3 [0,0,1, 1,1,2, 2,2,3]
    printMatrix sm1 3 3

    putStrLn "\n--- Test: get ---"
    print $ get sm1 0 0  -- debe dar 1
    print $ get sm1 1 1  -- debe dar 2
    print $ get sm1 0 1  -- debe dar 0

    putStrLn "\n--- Test: set ---"
    let sm2 = set sm1 0 1 9
    printMatrix sm2 3 3
    putStrLn "\n"
    let sm3 = set sm2 1 1 0  -- eliminación
    printMatrix sm3 3 3

    putStrLn "\n--- Test: transpose ---"
    let smT = transpose sm1
    printMatrix smT 3 3

    putStrLn "\n--- Test: add ---"
    let sm4 = fromList 3 3 [0,0,5, 1,2,4, 2,2,-3]
    let smSum = add sm1 sm4
    printMatrix smSum 3 3

-- Imprime una matriz completa, incluidas las posiciones con 0
printMatrix :: SparseMatrix -> Int -> Int -> IO ()
printMatrix sm rows cols = mapM_ putStrLn [fila r | r <- [0..rows-1]]
  where
    fila r = unwords [show (get sm r c) | c <- [0..cols-1]]

