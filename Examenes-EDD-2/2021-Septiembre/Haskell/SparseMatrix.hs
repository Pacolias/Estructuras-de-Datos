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
value (SM _ _ dict) index = fromMaybe 0 (D.valueOf index dict)

-- Función privada: actualiza un valor dado un índice
update :: SparseMatrix -> Index -> Int -> SparseMatrix
update sm@(SM fil col dict) index value
    | value == 0    = if D.isDefinedAt index dict then SM fil col (D.delete index dict) else sm 
    | otherwise     = SM fil col (D.updateOrInsert index (const value) value dict)

-- Función privada: construye un índice con validación
index :: SparseMatrix -> Int -> Int -> Index
index sm@(SM filMax colMax dict) fil col
    | fil < 0 || col < 0 || fil > filMax || col > colMax = error "Illegal index for the given matrix"
    | otherwise = Idx fil col

-- Función pública: actualiza el valor en una posición (valida el índice)
set :: SparseMatrix -> Int -> Int -> Int -> SparseMatrix
set sm fil col = update sm (index sm fil col) 

-- Función pública: obtiene el valor de una posición (valida el índice)
get :: SparseMatrix -> Int -> Int -> Int
get sm fil col = value sm (index sm fil col)

-- Suma de dos matrices dispersas
add :: SparseMatrix -> SparseMatrix -> SparseMatrix
add (SM fil1 col1 dict1) (SM fil2 col2 dict2)
    | fil1 /= fil2 || col1 /= col2  = error "The given matrices has not the same dimension"
    | otherwise                     = SM fil1 col1 (addDictionaries dict1 (D.keysValues dict2))
        where
            addDictionaries :: D.Dictionary Index Int -> [(Index,Int)] -> D.Dictionary Index Int
            addDictionaries dict []                 = dict
            addDictionaries dict ((idx,value):xs)   =
                case D.valueOf idx dict of 
                    Nothing -> D.insert idx value (addDictionaries dict xs)
                    Just v  -> 
                        if v + value == 0 
                            then D.delete idx (addDictionaries dict xs) 
                            else D.updateOrInsert idx (const (v + value)) (v + value) (addDictionaries dict xs)

-- Traspuesta de una matriz dispersa
transpose :: SparseMatrix -> SparseMatrix
transpose sm@(SM fil col dict) = foldr (\(Idx x y, value) acc -> set acc y x value) (sparseMatrix col fil) (D.keysValues dict)

-- Crea una matriz dispersa a partir de una lista [fila, col, val, fila, col, val, ...]
fromList :: Int -> Int -> [Int] -> SparseMatrix
fromList filMax colMax list
    | length list `mod` 3 /= 0  = error "The length of the given list is not multiple of 3"
    | otherwise                 = foldr (\(fil,col,val) acc -> set acc fil col val) (sparseMatrix filMax colMax) (toTriplets list)
        where 
            toTriplets :: [Int] -> [(Int,Int,Int)]
            toTriplets []           = []
            toTriplets (x:y:z:xs)   = (x,y,z) : toTriplets xs
-- Complejidad esperada: O(n log k), donde n = longitud de la lista / 3, k = elementos distintos insertados.
