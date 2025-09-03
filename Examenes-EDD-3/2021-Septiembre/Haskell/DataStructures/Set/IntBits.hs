-- Pepe Gallardo, UMA

module DataStructures.Set.IntBits (
    empty,
    isEmpty,
    setBit',
    clearBit',
    bitValue,
    countBits,
    toList
  ) where

import Data.Bits
import Data.List (foldl')

type BitSet = Int

-- | BitSet vacío (todos los bits a 0)
empty :: BitSet
empty = 0

-- | Comprueba si todos los bits están a 0
isEmpty :: BitSet -> Bool
isEmpty bs = bs == 0

-- | Establece el bit en la posición dada a 1
setBit' :: Int -> BitSet -> BitSet
setBit' i bs = bs .|. bit i

-- | Establece el bit en la posición dada a 0
clearBit' :: Int -> BitSet -> BitSet
clearBit' i bs = bs .&. complement (bit i)

-- | Devuelve True si el bit en la posición dada es 1
bitValue :: Int -> BitSet -> Bool
bitValue i bs = testBit bs i

-- | Cuenta cuántos bits están a 1
countBits :: BitSet -> Int
countBits = popCount

-- | Devuelve una lista con los índices de los bits que están a 1
toList :: BitSet -> [Int]
toList bs = [i | i <- [0..63], testBit bs i]

