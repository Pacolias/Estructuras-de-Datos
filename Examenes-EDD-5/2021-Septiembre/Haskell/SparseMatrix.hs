module SparseMatrix (
    SparseMatrix,
    sparseMatrix,
    set,
    get,
    add,
    transpose,
    fromList
) where

import qualified DataStructures.Dictionary.AVLDictionary as D -- Asumiendo que tienes un módulo de diccionario AVL
import Data.Maybe (fromMaybe)

-- Representación de un índice de matriz
data Index = Idx Int Int deriving (Eq, Ord, Show)

-- Tipo de datos para representar una matriz dispersa
data SparseMatrix = SM Int Int (D.Dictionary Index Int)

-- Construye una matriz nula de dimensiones dadas
sparseMatrix :: Int -> Int -> SparseMatrix
sparseMatrix fil col = SM fil col D.empty

-- Función privada: obtiene el valor en un índice dado
value :: SparseMatrix -> Index -> Int
value (SM _ _ dict) idx = fromMaybe 0 (D.valueOf idx dict)

-- Función privada: actualiza un valor dado un índice
update :: SparseMatrix -> Index -> Int -> SparseMatrix
update (SM fil col dict) idx val
    | val == 0  = SM fil col (D.delete idx dict)
    | otherwise = SM fil col (D.updateOrInsert idx (const val) val dict)

-- Función privada: construye un índice con validación
index :: SparseMatrix -> Int -> Int -> Index
index (SM filMax colMax _) fil col
    | fil < 0 || col < 0 || fil >= filMax || col >= colMax = error "Index out of bounds"
    | otherwise                                            = Idx fil col

-- Función pública: actualiza el valor en una posición (valida el índice)
set :: SparseMatrix -> Int -> Int -> Int -> SparseMatrix
set sm fil col = update sm (index sm fil col)

-- Función pública: obtiene el valor de una posición (valida el índice)
get :: SparseMatrix -> Int -> Int -> Int
get sm fil col = value sm (index sm fil col)

-- Suma de dos matrices dispersas
add :: SparseMatrix -> SparseMatrix -> SparseMatrix
add sm1@(SM fil1 col1 dict1) (SM fil2 col2 dict2)
    | fil1 /= fil2 || col1 /= col2 = error "add on matrices with different dimension"
    | otherwise                    = foldr (\(idx, val) acc -> update acc idx (val + value acc idx)) sm1 (D.keysValues dict2) 

-- Traspuesta de una matriz dispersa
transpose :: SparseMatrix -> SparseMatrix
transpose (SM fil col dict) = foldr (\(Idx f c, val) acc -> update acc (Idx c f) val) (sparseMatrix col fil) (D.keysValues dict)

-- Crea una matriz dispersa a partir de una lista [fila, col, val, fila, col, val, ...]
fromList :: Int -> Int -> [Int] -> SparseMatrix
fromList fil col xs
    | length xs `mod` 3 /= 0 = error "The length of the list is not multiple of 3"
    | otherwise              = fromListRec xs
        where 
            fromListRec []         = sparseMatrix fil col
            fromListRec (x:y:z:xs) = set (fromListRec xs) x y z

-- Complejidad esperada: O(n log k), donde n = longitud de la lista / 3, k = elementos distintos insertados.
