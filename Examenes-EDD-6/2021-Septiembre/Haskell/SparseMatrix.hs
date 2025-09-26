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
    | fil < 0 || col < 0 || fil >= filMax || col >= colMax = error "wrong rows or columns number"
    | otherwise                                            = Idx fil col

-- Función pública: actualiza el valor en una posición (valida el índice)
set :: SparseMatrix -> Int -> Int -> Int -> SparseMatrix
set sm fil col val = update sm (index sm fil col) val

-- Función pública: obtiene el valor de una posición (valida el índice)
get :: SparseMatrix -> Int -> Int -> Int
get sm fil col = value sm (index sm fil col)

-- Suma de dos matrices dispersas
add :: SparseMatrix -> SparseMatrix -> SparseMatrix
add sm1@(SM fil1 col1 dict1) sm2@(SM fil2 col2 dict2)
    | fil1 /= fil2 || col1 /= col2 = error "add on matrices with different dimension"
    | otherwise                    = foldr (\idx mat -> update mat idx (value sm1 idx + value sm2 idx)) sm1 (D.keys dict2)

-- Traspuesta de una matriz dispersa
transpose :: SparseMatrix -> SparseMatrix
transpose (SM fil col dict) = SM col fil (foldr (\((Idx fil col), val) -> D.insert (Idx col fil) val) D.empty (D.keysValues dict))

-- Crea una matriz dispersa a partir de una lista [fila, col, val, fila, col, val, ...]
fromList :: Int -> Int -> [Int] -> SparseMatrix
fromList fil col xs 
    | length xs `mod` 3 /= 0 = error "fromList on list with lenght not multiple of three"
    | otherwise              = SM fil col (fromListRec xs)
        where 
            fromListRec []         = D.empty 
            fromListRec (x:y:z:xs) = D.insert (Idx x y) z (fromListRec xs) 

-- Complejidad esperada: O(n log k), donde n = longitud de la lista / 3, k = elementos distintos insertados.
