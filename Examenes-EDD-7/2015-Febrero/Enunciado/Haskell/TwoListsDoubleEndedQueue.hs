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
empty = undefined

-- Complexity: O(1)
isEmpty :: DEQue a -> Bool
isEmpty = undefined

-- Complexity: O(1)
addFirst :: a -> DEQue a -> DEQue a
addFirst = undefined

-- Complexity: O(1)
addLast :: a -> DEQue a -> DEQue a
addLast = undefined

-- Complexity: O(1)
first :: DEQue a -> a
first = undefined

-- Complexity: O(1)
last :: DEQue a -> a
last = undefined

-- Complexity: O(1)
deleteFirst :: DEQue a -> DEQue a
deleteFirst = undefined

-- Complexity: O(1)
deleteLast :: DEQue a -> DEQue a
deleteLast = undefined

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
