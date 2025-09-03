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
isEmpty _ = False

-- Complexity: O(1)
addFirst :: a -> DEQue a -> DEQue a
addFirst x (DEQ lista1 lista2) = DEQ (x:lista1) lista2

-- Complexity: O(1)
addLast :: a -> DEQue a -> DEQue a
addLast x (DEQ lista1 lista2) = DEQ lista1 (x:lista2)

-- Complexity: O(n)
first :: DEQue a -> a
first (DEQ (x:xs) _) = x
first (DEQ [] []) = error "first on empty queue"
first (DEQ [] ys) = first (rebalanceaLista ys)
   where 
      rebalanceaLista :: [a] -> DEQue a 
      rebalanceaLista zs = DEQ (reverse frontal) trasera 
         where 
            (frontal, trasera) = splitAt (length zs `div` 2) zs

-- Complexity: O(n)
last :: DEQue a -> a
last (DEQ _ (y:ys)) = y
last (DEQ [] []) = error "last on empty queue"
last (DEQ xs []) = last (rebalanceaLista xs)
   where 
      rebalanceaLista :: [a] -> DEQue a 
      rebalanceaLista zs = DEQ frontal (reverse trasera) 
         where 
            (frontal, trasera) = splitAt (length zs `div` 2) zs

-- Complexity: O(n)
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ (x:xs) ys) = DEQ xs ys 
deleteFirst (DEQ [] []) = error "deleteFirst on empty queue"
deleteFirst (DEQ [] ys) = deleteFirst (rebalanceaLista ys)
   where 
      rebalanceaLista :: [a] -> DEQue a 
      rebalanceaLista zs = DEQ (reverse frontal) trasera 
         where 
            (frontal, trasera) = splitAt (length zs `div` 2) zs

-- Complexity: O(n)
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ xs (y:ys)) = DEQ xs ys 
deleteLast (DEQ [] []) = error "deleteLast on empty queue"
deleteLast (DEQ xs []) = deleteLast (rebalanceaLista xs)
   where 
      rebalanceaLista :: [a] -> DEQue a 
      rebalanceaLista zs = DEQ frontal (reverse trasera) 
         where 
            (frontal, trasera) = splitAt (length zs `div` 2) zs

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

-------------------------------------------------------------------------------
-- Test unitarios
test1 = isEmpty empty == True
test2 = isEmpty (addFirst 1 empty) == False
test3 = isEmpty (addLast 1 empty) == False

test4 = first (addFirst 1 (addLast 2 empty)) == 1
test5 = last  (addFirst 1 (addLast 2 empty)) == 2

test6 = toList (addFirst 1 (addLast 2 (addFirst 0 empty))) == [1,0,2]

test7 = toList (deleteFirst (addFirst 1 (addLast 2 empty))) == [2]
test8 = toList (deleteLast (addFirst 1 (addLast 2 empty))) == [1]

test9 = toList (deleteFirst (deleteFirst (addFirst 2 (addFirst 1 empty)))) == []
test10 = toList (deleteLast (deleteLast (addLast 1 (addLast 2 empty)))) == []
