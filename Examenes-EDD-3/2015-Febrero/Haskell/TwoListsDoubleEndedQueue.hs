-------------------------------------------------------------------------------
-- Estructuras de Datos. UMA
--
-- Implementación del TAD Deque
--
-- Apellidos: Molina Cuenca
-- Nombre: Paco
-- Doble Grado Matemáticas e Ingeniería Informática
-------------------------------------------------------------------------------

module TwoListsDoubleEndedQueue
   ( DEQue
   , empty
   , isEmpty
   , first
   , last
   , addFirst
   , addLast
   , deleteFirst
   , deleteLast
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a]

-- Complexity: O(1)
empty :: DEQue a
empty = DEQ [] []

-- Complexity: O(1)
isEmpty :: DEQue a -> Bool
isEmpty (DEQ [] []) = True
isEmpty _           = False

-- Complexity: O(1)
addFirst :: a -> DEQue a -> DEQue a
addFirst x (DEQ xs ys) = DEQ (x:xs) ys

-- Complexity: O(1)
addLast :: a -> DEQue a -> DEQue a
addLast y (DEQ xs ys) = DEQ xs (y:ys)

-- Complexity: O(1)
first :: DEQue a -> a 
first (DEQ [] [])    = error "first on empty list"
first (DEQ (x:xs) _) = x 
first (DEQ [] ys)    = first (DEQ lista1 lista2)
   where 
      (lista1, lista2) = splitAt (length ys `div` 2) ys

-- Complexity: O(1)
last :: DEQue a -> a
last (DEQ [] [])    = error "last on empty list"
last (DEQ _ (y:ys)) = y 
last (DEQ xs [])    = last (DEQ lista1 lista2)
   where 
      (lista1, lista2) = splitAt (length xs `div` 2) xs

-- Complexity: O(1)
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ [] [])     = error "deleteFirst on empty list"
deleteFirst (DEQ (x:xs) ys) = DEQ xs ys
deleteFirst (DEQ [] ys)     = deleteFirst (DEQ lista1 lista2)
   where 
      (lista1, lista2) = splitAt (length ys `div` 2) ys

-- Complexity: O(1)
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ [] [])     = error "deleteLast on empty list"
deleteLast (DEQ xs (y:ys)) = DEQ xs ys
deleteLast (DEQ xs [])     = deleteLast (DEQ lista1 lista2)
   where 
      (lista1, lista2) = splitAt (length xs `div` 2) xs
-------------------------------------------------------------------------------

instance (Show a) => Show (DEQue a) where
   show q = "TwoListsDoubleEndedQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

toList :: DEQue a -> [a]
toList (DEQ xs ys) =  xs ++ reverse ys

instance (Eq a) => Eq (DEQue a) where
   q == q' =  toList q == toList q'

instance (Arbitrary a) => Arbitrary (DEQue a) where
   arbitrary =  do
      xs <- listOf arbitrary
      ops <- listOf (oneof [return addFirst, return addLast])
      return (foldr id empty (zipWith ($) ops xs))
