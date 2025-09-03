-------------------------------------------------------------------------------
-- Apellidos, Nombre: Paco Molina Cuenca
-- Titulacion, Grupo: Doble Grado Matemáticas e Ingeniería Informática
--
-- Estructuras de Datos. Grados en Informatica. UMA.
-------------------------------------------------------------------------------

module AVLBiDictionary( BiDictionary
                      , empty
                      , isEmpty
                      , size
                      , insert
                      , valueOf
                      , keyOf
                      , deleteByKey
                      , deleteByValue
                      , toBiDictionary
                      , compose
                      , isPermutation
                      , orbitOf
                      , cyclesOf
                      ) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.Set.BSTSet               as S

import           Data.List                               (intercalate, nub, sort,
                                                          (\\))
import           Data.Maybe                              (fromJust, fromMaybe,
                                                          isJust)
import           Test.QuickCheck


data BiDictionary a b = Bi (D.Dictionary a b) (D.Dictionary b a)

-- | Exercise a. empty, isEmpty, size

empty :: (Ord a, Ord b) => BiDictionary a b
empty = Bi D.empty D.empty

isEmpty :: (Ord a, Ord b) => BiDictionary a b -> Bool
isEmpty (Bi dk dv) = D.isEmpty dk && D.isEmpty dv

size :: (Ord a, Ord b) => BiDictionary a b -> Int
size (Bi dk _) = D.size dk
 
-- | Exercise b. insert

insert :: (Ord a, Ord b) => a -> b -> BiDictionary a b -> BiDictionary a b
insert k v (Bi dk dv) = Bi (D.updateOrInsert k (const v) v dk) (D.updateOrInsert v (const k) k dv)

-- | Exercise c. keyOf

keyOf :: (Ord a, Ord b) => b -> BiDictionary a b -> Maybe a
keyOf v (Bi _ dv) = D.valueOf v dv

-- | Exercise d. valueOf

valueOf :: (Ord a, Ord b) => a -> BiDictionary a b -> Maybe b
valueOf k (Bi dk _) = D.valueOf k dk

-- | Exercise e. deleteByKey

deleteByKey :: (Ord a, Ord b) => a -> BiDictionary a b -> BiDictionary a b
deleteByKey k biDict@(Bi dk dv) =
  case valueOf k biDict of
    Nothing -> biDict 
    Just v  -> Bi (D.delete k dk) (D.delete v dv)
      
-- | Exercise f. deleteByValue

deleteByValue :: (Ord a, Ord b) => b -> BiDictionary a b -> BiDictionary a b
deleteByValue v biDict@(Bi dk dv) =
  case keyOf v biDict of
    Nothing -> biDict 
    Just k  -> Bi (D.delete k dk) (D.delete v dv)

-- | Exercise g. toBiDictionary

toBiDictionary :: (Ord a, Ord b) => D.Dictionary a b -> BiDictionary a b
toBiDictionary dict
  | isInjective dict = Bi dict (reverseDictionary (D.keys dict) (D.values dict) D.empty)
  | otherwise = error "El diccionario pasado como parametro no es inyectivo"
    where 
      isInjective :: (Ord a, Ord b) => D.Dictionary a b -> Bool 
      isInjective dict = length (nub (D.values dict)) == length (D.keys dict)

      reverseDictionary :: (Ord a, Ord b) => [a] -> [b] -> D.Dictionary b a -> D.Dictionary b a 
      reverseDictionary [] [] dict = dict 
      reverseDictionary (k:ks) (v:vs) dict = D.insert v k (reverseDictionary ks vs dict)

-- | Exercise h. compose

compose :: (Ord a, Ord b, Ord c) => BiDictionary a b -> BiDictionary b c -> BiDictionary a c
compose bd1@(Bi dk1 _) bd2@(Bi dk2 _) = composeRec (D.keys dk1) empty
  where 
    composeRec [] biDict = biDict 
    composeRec (a:as) bd@(Bi dk dv) = 
      case valueOf a bd1 of 
        Nothing -> composeRec as bd
        Just b  ->
          case valueOf b bd2 of
            Nothing -> composeRec as bd 
            Just c -> composeRec as (Bi (D.insert a c dk) (D.insert c a dv))


-- | Exercise i. isPermutation

isPermutation :: Ord a => BiDictionary a a -> Bool
isPermutation (Bi dk _) = sort (D.keys dk) == sort (D.values dk)

-- |------------------------------------------------------------------------


-- | Exercise j. orbitOf

orbitOf :: Ord a => a -> BiDictionary a a -> [a]
orbitOf k biDict@(Bi dk _)
  | isPermutation biDict = 
      case valueOf k biDict of 
        Nothing -> []
        Just v  -> calcularOrbita v [k]
  | otherwise = error "El diccionario pasado como parametro no es una permutacion"
  where 
    calcularOrbita clave lista
      | clave == k = reverse lista
      | otherwise =
        case valueOf clave biDict of 
          Nothing     -> reverse (clave : lista)
          Just valor  -> calcularOrbita valor (clave : lista)

-- Caso de prueba
-- Diccionario base: 1->2, 2->3, 3->1 (una permutación)
bd1 :: BiDictionary Int Int
bd1 = toBiDictionary $ D.insert 1 2 $
                        D.insert 2 3 $
                        D.insert 3 1 $
                        D.empty

-- Test: la órbita de 1 debe ser [1,2,3]
testOrbit = orbitOf 1 bd1

-- | Exercise k. cyclesOf

cyclesOf :: Ord a => BiDictionary a a -> [[a]]
cyclesOf biDict@(Bi dk _)
  | isPermutation biDict = nub (calcularCiclos (D.keys dk) [])
  | otherwise = error "El diccionario pasado como parametro no es una permutacion"
  where 
    calcularCiclos [] lista = lista 
    calcularCiclos (k:ks) lista = (sort ciclo) : (calcularCiclos ks lista)
      where 
        ciclo = orbitOf k biDict

-- Caso de prueba
-- Diccionario con 4 ciclos
bd2 :: BiDictionary Int Int
bd2 = toBiDictionary $
         D.insert 1 2 $
         D.insert 2 1 $                   -- ciclo [1,2]
         D.insert 3 4 $
         D.insert 4 5 $
         D.insert 5 3 $                   -- ciclo [3,4,5]
         D.insert 6 6 $                   -- ciclo [6]
         D.insert 7 8 $
         D.insert 8 9 $
         D.insert 9 10 $
         D.insert 10 7 $
         D.empty                          -- ciclo [7,8,9,10]

testCycles = cyclesOf bd2


-- |------------------------------------------------------------------------


instance (Show a, Show b) => Show (BiDictionary a b) where
  show (Bi dk dv)  = "BiDictionary(" ++ intercalate "," (aux (D.keysValues dk)) ++ ")"
                        ++ "(" ++ intercalate "," (aux (D.keysValues dv)) ++ ")"
   where
    aux kvs  = map (\(k,v) -> show k ++ "->" ++ show v) kvs
