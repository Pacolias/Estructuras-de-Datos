module SparseMatrix (
    SparseMatrix,
    sparseMatrix,
    set,
    get,
    add,
    transpose,
    fromList
) where

import qualified AVLDictionary as D -- Asumiendo que tienes un módulo de diccionario AVL
import Data.Maybe (fromMaybe)

-- Representación de un índice de matriz
data Index = Idx Int Int deriving (Eq, Ord, Show)

-- Tipo de datos para representar una matriz dispersa
data SparseMatrix = SM {
    rows :: Int,
    cols :: Int,
    dict :: D.Dictionary Index Int
}

-- Construye una matriz nula de dimensiones dadas
sparseMatrix :: Int -> Int -> SparseMatrix
sparseMatrix = undefined

-- Función privada: obtiene el valor en un índice dado
value :: SparseMatrix -> Index -> Int
value = undefined

-- Función privada: actualiza un valor dado un índice
update :: SparseMatrix -> Index -> Int -> SparseMatrix
update = undefined

-- Función privada: construye un índice con validación
index :: SparseMatrix -> Int -> Int -> Index
index = undefined

-- Función pública: actualiza el valor en una posición (valida el índice)
set :: SparseMatrix -> Int -> Int -> Int -> SparseMatrix
set = undefined

-- Función pública: obtiene el valor de una posición (valida el índice)
get :: SparseMatrix -> Int -> Int -> Int
get = undefined

-- Suma de dos matrices dispersas
add :: SparseMatrix -> SparseMatrix -> SparseMatrix
add = undefined

-- Traspuesta de una matriz dispersa
transpose :: SparseMatrix -> SparseMatrix
transpose = undefined

-- Crea una matriz dispersa a partir de una lista [fila, col, val, fila, col, val, ...]
fromList :: Int -> Int -> [Int] -> SparseMatrix
fromList = undefined
-- Complejidad esperada: O(n log k), donde n = longitud de la lista / 3, k = elementos distintos insertados.
