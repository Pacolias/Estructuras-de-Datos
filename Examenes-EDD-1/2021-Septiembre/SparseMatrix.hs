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
sparseMatrix fil col 
    | fil <= 0 || col <= 0 = error "Error de dimensiones"
    | otherwise = SM fil col D.empty

-- Función privada: obtiene el valor en un índice dado
value :: SparseMatrix -> Index -> Int
value (SM fil col diccionario) idx  = 
    case D.valueOf idx diccionario of 
        Nothing     -> 0
        Just valor  -> valor

-- Función privada: actualiza un valor dado un índice
update :: SparseMatrix -> Index -> Int -> SparseMatrix
update (SM fil col diccionario) idx nuevoValor 
    | nuevoValor == 0 = SM fil col (D.delete idx diccionario)
    | otherwise = SM fil col (D.updateOrInsert idx (const nuevoValor) nuevoValor diccionario)

-- Función privada: construye un índice con validación
index :: SparseMatrix -> Int -> Int -> Index
index (SM filMatriz colMatriz _) fil col 
    | fil <= 0 || fil > filMatriz || col <= 0 || col > colMatriz = error "Error: Dimensiones no validas"
    | otherwise = Idx fil col

-- Función pública: actualiza el valor en una posición (valida el índice)
set :: SparseMatrix -> Int -> Int -> Int -> SparseMatrix
set sm fil col valor = update sm (index sm fil col) valor

-- Función pública: obtiene el valor de una posición (valida el índice)
get :: SparseMatrix -> Int -> Int -> Int
get sm fil col  = value sm (index sm fil col)

-- Suma de dos matrices dispersas
add :: SparseMatrix -> SparseMatrix -> SparseMatrix
add (SM fil1 col1 diccionario1) (SM fil2 col2 diccionario2)
 | fil1 == fil2 && col1 == col2 = SM fil1 col1 (sumarDiccionarios listaIndices)
 | otherwise = error "No se pueden sumar matrices de distinta dimension"
 where 
    listaIndices = nub ((D.keys diccionario1) ++ (D.keys diccionario2))

    sumarDiccionarios :: [Index] -> Dictionary
    sumarDiccionarios [] = D.empty 
    sumarDiccionarios (i:is) 
        | (D.isDefinedAt i diccionario1) && (D.isDefinedAt i diccionario2) && valor /= 0 = D.insertOrUpdate i (const valor) valor (sumarDiccionarios is)
        | D.isDefinedAt i diccionario1 = D.insertOrUpdate i (const valor1) valor1 (sumarDiccionarios is)
        | D.isDefinedAt i diccionario2 = D.insertOrUpdate i (const valor2) valor2 (sumarDiccionarios is)
        | otherwise = sumarDiccionarios is
        where 
            valor1 = D.valueOf i diccionario1
            valor2 = D.valueOf i diccionario2
            valor = valor1 + valor2


-- Traspuesta de una matriz dispersa
transpose :: SparseMatrix -> SparseMatrix
transpose (SM fil col diccionario) = SM fil col (diccionarioSimetrico D.keys diccionario)
    where
        diccionarioSimetrico :: [Index] -> Dictionary
        diccionarioSimetrico [] = D.empty 
        diccionarioSimetrico (k:ks) = D.insert (invertirIndice k) (D.valueOf k diccionario) (diccionarioSimetrico ks)
            where 
                invertirIndice :: Index -> Index 
                invertirIndice (Idx fil col) = Idx col fil

-- Crea una matriz dispersa a partir de una lista [fila, col, val, fila, col, val, ...]
fromList :: Int -> Int -> [Int] -> SparseMatrix
fromList fil col lista 
    | length lista `mod` 3 /= 0 = error "La lista no tiene un numero impar de elementos"
    | otherwise = insertaLista (sparseMatrix fil col) lista 
        where 
            insertaLista :: SparseMatrix -> [Int] -> SparseMatrix 
            insertaLista sm [] = sm 
            insertaLista sm (fil:col:val:xs) = update (insertaLista sm xs) (Index fil col) val

-- Complejidad esperada: O(n log k), donde n = longitud de la lista / 3, k = elementos distintos insertados.