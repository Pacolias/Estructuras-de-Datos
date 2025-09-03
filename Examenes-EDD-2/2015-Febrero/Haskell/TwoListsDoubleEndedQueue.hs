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
first (DEQ [] [])    = error "first on empty queue"
first (DEQ [] ys)    = first (DEQ (reverse (drop (div (length ys) 2) ys)) (take (div (length ys) 2) ys))
first (DEQ (x:xs) _) = x

-- Complexity: O(1)
last :: DEQue a -> a
last (DEQ [] [])     = error "last on empty queue"
last (DEQ xs [])     = last (DEQ (reverse (drop (div (length xs) 2) xs)) (take (div (length xs) 2) xs))
last (DEQ _ (y:ys))  = y

-- Complexity: O(1)
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ [] [])     = error "deleteFirst on empty queue"
deleteFirst (DEQ [] ys)     = deleteFirst (DEQ (reverse (drop (div (length ys) 2) ys)) (take (div (length ys) 2) ys))
deleteFirst (DEQ (x:xs) ys) = DEQ xs ys

-- Complexity: O(1)
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ [] [])     = error "last on empty queue"
deleteLast (DEQ xs [])     = deleteLast (DEQ (reverse (drop (div (length xs) 2) xs)) (take (div (length xs) 2) xs))
deleteLast (DEQ xs (y:ys)) = DEQ xs ys
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
