-------------------------------------------------------------------------------
-- Axioms for a DEQue (Double Ended Queue)
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, Blas Ruiz, Enero 2015
-------------------------------------------------------------------------------

module DoubleEndedQueueAxioms(dqAxioms) where

import Prelude hiding (last)
import Test.QuickCheck

import TwoListsDoubleEndedQueue

-- Axiomas básicos
ax1_1 x    = property $ first (addLast x empty) == x
ax1_2 x q  = property $ last  (addLast x q)     == x
ax1_3 x    = property $ last  (addFirst x empty) == x
ax1_4 x q  = property $ first (addFirst x q)    == x

ax1_5 x    = property $ deleteFirst (addLast x empty) == empty
ax1_6 x q  = property $ deleteFirst (addFirst x q)    == q

ax1_61 q   = several q ==> deleteFirst (deleteLast q) == deleteLast (deleteFirst q)
ax1_62 x y q = property $ addLast x (addFirst y q) == addFirst y (addLast x q)

-- Definiciones auxiliares
several  q = not (isEmpty q) && not (isEmpty (deleteLast q))
several' q = not (isEmpty q) && not (isEmpty (deleteFirst q))

ax5_1 q  = property $ several q == several' q

ax1_7 x q = property $ deleteLast (addLast x q) == q
ax1_8 x   = property $ deleteLast (addFirst x empty) == empty

-- Axiomas con condiciones reales
ax2_1 x q   = not (isEmpty q) ==> first q == first (addLast x q)
ax2_11 q    = several q       ==> first q == first (deleteLast q)

ax2_2 x q   = not (isEmpty q) ==> last q == last (addFirst x q)
ax2_21 q    = several q       ==> last q == last (deleteFirst q)

ax3_1 x q   = not (isEmpty q) ==> deleteFirst (addLast x q) == addLast x (deleteFirst q)
ax3_2 x q   = not (isEmpty q) ==> deleteLast (addFirst x q) == addFirst x (deleteLast q)

-- Axiomas sobre vacío
ax4_1       = property $ isEmpty empty
ax4_2 x q   = property $ not (isEmpty (addFirst x q))
ax4_3 x q   = property $ not (isEmpty (addLast  x q))


-- Tipo de elemento base
type E = Int

-- Ejecución de todas las propiedades
dqAxioms = do
  quickCheck (ax1_1 :: E -> Property)
  quickCheck (ax1_2 :: E -> DEQue E -> Property)
  quickCheck (ax1_3 :: E -> Property)
  quickCheck (ax1_4 :: E -> DEQue E -> Property)

  quickCheck (ax1_5 :: E -> Property)
  quickCheck (ax1_6 :: E -> DEQue E -> Property)
  quickCheck (ax1_61 :: DEQue E -> Property)
  quickCheck (ax1_62 :: E -> E -> DEQue E -> Property)

  quickCheck (ax1_7 :: E -> DEQue E -> Property)
  quickCheck (ax1_8 :: E -> Property)

  quickCheck (ax2_1 :: E -> DEQue E -> Property)
  quickCheck (ax2_11 :: DEQue E -> Property)
  quickCheck (ax2_2 :: E -> DEQue E -> Property)
  quickCheck (ax2_21 :: DEQue E -> Property)

  quickCheck (ax3_1 :: E -> DEQue E -> Property)
  quickCheck (ax3_2 :: E -> DEQue E -> Property)

  quickCheck (ax4_1 :: Property)
  quickCheck (ax4_2 :: E -> DEQue E -> Property)
  quickCheck (ax4_3 :: E -> DEQue E -> Property)

  quickCheck (ax5_1 :: DEQue E -> Property)
