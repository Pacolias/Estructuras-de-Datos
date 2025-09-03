module Main where

import AVL

-- Función auxiliar para imprimir una lista de cubos bonita
printBins :: [Bin] -> IO ()
printBins = mapM_ print

-- Ejemplo de pesos
exampleWeights :: [Weight]
exampleWeights = [4, 8, 1, 4, 2, 1, 7, 2, 5, 3]

-- Capacidad de cada cubo
capacity :: Capacity
capacity = 10

main :: IO ()
main = do
  putStrLn "=== Prueba de Bin Packing AVL ==="
  let avl = addAll capacity exampleWeights
  putStrLn $ "Se han añadido " ++ show (length exampleWeights) ++ " objetos"
  putStrLn $ "Número de cubos usados (AVL): " ++ show (length $ toList avl)
  putStrLn "Contenido de los cubos (AVL):"
  printBins (toList avl)

  putStrLn "\n=== Prueba de Bin Packing Lineal ==="
  let seq = linearBinPacking capacity exampleWeights
  putStrLn $ "Número de cubos usados (Lineal): " ++ show (length $ seqToList seq)
  putStrLn "Contenido de los cubos (Lineal):"
  printBins (seqToList seq)

  putStrLn "\n=== Prueba con addAllFold ==="
  let avlFold = addAllFold exampleWeights capacity
  putStrLn $ "Número de cubos usados (Fold): " ++ show (length $ toList avlFold)
  putStrLn "Contenido de los cubos (Fold):"
  printBins (toList avlFold)

  putStrLn "\n=== Pruebas individuales de funciones auxiliares ==="
  let bin0 = emptyBin capacity
  putStrLn $ "Cubo vacío: " ++ show bin0
  let bin1 = addObject 5 bin0
  putStrLn $ "Cubo tras añadir 5: " ++ show bin1
  let bin2 = addObject 3 bin1
  putStrLn $ "Cubo tras añadir 3: " ++ show bin2
  putStrLn $ "Capacidad restante: " ++ show (remainingCapacity bin2)

  putStrLn "\n=== Prueba de rotación ==="
  let binA = addObject 3 (emptyBin 10)
      binB = addObject 2 (emptyBin 10)
      binC = addObject 4 (emptyBin 10)
      avl1 = node binA Empty Empty
      avl2 = node binB Empty Empty
      avl3 = node binC avl1 avl2
  putStrLn "AVL antes de rotación izquierda:"
  print (toList avl3)
  let rotated = rotateLeft binC avl1 avl2
  putStrLn "AVL tras rotación izquierda:"
  print (toList rotated)

