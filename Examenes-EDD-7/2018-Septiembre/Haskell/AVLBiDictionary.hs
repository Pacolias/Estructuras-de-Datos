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

import           Data.List                               (intercalate, nub,
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
insert k v (Bi dk dv) = 
  case D.valueOf k dk of 
    Nothing -> Bi (D.insert k v dk) (D.insert v k dv)
    Just v' ->
      case D.valueOf v dv of 
        Nothing -> Bi (D.insert k v (D.delete k dk)) (D.insert v k (D.delete v' dv))
        Just k' -> Bi (D.insert k v (D.delete k (D.delete k' dk))) (D.insert v k (D.delete v (D.delete v' dv)))

-- | Exercise c. keyOf

keyOf :: (Ord a, Ord b) => b -> BiDictionary a b -> Maybe a
keyOf v (Bi _ dv) = D.valueOf v dv

-- | Exercise d. valueOf

valueOf :: (Ord a, Ord b) => a -> BiDictionary a b -> Maybe b
valueOf k (Bi dk _) = D.valueOf k dk

-- | Exercise e. deleteByKey

deleteByKey :: (Ord a, Ord b) => a -> BiDictionary a b -> BiDictionary a b
deleteByKey k biDict@(Bi dk dv) =
  case D.valueOf k dk of 
    Nothing -> biDict 
    Just v  -> Bi (D.delete k dk) (D.delete v dv)
      
-- | Exercise f. deleteByValue

deleteByValue :: (Ord a, Ord b) => b -> BiDictionary a b -> BiDictionary a b
deleteByValue v biDict@(Bi dk dv) =
  case D.valueOf v dv of 
    Nothing -> biDict 
    Just k  -> Bi (D.delete k dk) (D.delete v dv)

-- | Exercise g. toBiDictionary

toBiDictionary :: (Ord a, Ord b) => D.Dictionary a b -> BiDictionary a b
toBiDictionary dict 
  | length (D.keys dict) /= length (nub (D.values dict)) = error "toBidictionary on non injective dictionary"
  | otherwise                                            = Bi dict (foldr (\(k,v) -> D.insert v k) D.empty (D.keysValues dict))

-- | Exercise h. compose

compose :: (Ord a, Ord b, Ord c) => BiDictionary a b -> BiDictionary b c -> BiDictionary a c
compose (Bi dk1 _) (Bi dk2 _) = toBiDictionary (composeRec (D.keys dk1))
  where 
    composeRec [] = D.empty 
    composeRec (a:as) = 
      case D.valueOf a dk1 of 
        Nothing -> composeRec as
        Just b  -> 
          case D.valueOf b dk2 of 
            Nothing -> composeRec as 
            Just c  -> D.insert a c (composeRec as)

-- | Exercise i. isPermutation

isPermutation :: Ord a => BiDictionary a a -> Bool
isPermutation (Bi dk _) = null (D.keys dk \\ D.values dk) && null (D.values dk \\ D.keys dk)

-- |------------------------------------------------------------------------


-- | Exercise j. orbitOf

orbitOf :: Ord a => a -> BiDictionary a a -> [a]
orbitOf x biDict@(Bi dk _) 
  | not (isPermutation biDict) = error "orbitOf on non permutation biDictionary" 
  | otherwise                  = orbitOfRec x []
    where 
      orbitOfRec y sol =
        case D.valueOf y dk of 
          Nothing -> error ""
          Just z  -> if z == x then y:sol else orbitOfRec z (y:sol)

-- | Exercise k. cyclesOf

cyclesOf :: Ord a => BiDictionary a a -> [[a]]
cyclesOf biDict@(Bi dk _) 
  | not (isPermutation biDict) = error "orbitOf on non permutation biDictionary" 
  | otherwise                  = cyclesOfRec (D.keys dk)
    where 
      cyclesOfRec []     = []
      cyclesOfRec (k:ks) = ciclo : cyclesOfRec (ks \\ ciclo)
        where 
          ciclo = orbitOf k biDict 

-- |------------------------------------------------------------------------


instance (Show a, Show b) => Show (BiDictionary a b) where
  show (Bi dk dv)  = "BiDictionary(" ++ intercalate "," (aux (D.keysValues dk)) ++ ")"
                        ++ "(" ++ intercalate "," (aux (D.keysValues dv)) ++ ")"
   where
    aux kvs  = map (\(k,v) -> show k ++ "->" ++ show v) kvs
