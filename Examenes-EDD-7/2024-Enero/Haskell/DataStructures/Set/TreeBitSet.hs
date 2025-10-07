--Student's name: Paco Molina Cuenca
--Student's group: 
--Identity number (DNI if Spanish/passport if Erasmus):

module DataStructures.Set.TreeBitSet (
  TreeBitSet,
  empty,
  size,
  capacity,
  isEmpty,
  contains,
  add,
  toList

  ) where

import Test.QuickCheck
import qualified DataStructures.Set.IntBits as IntBits
import Data.List((\\))


data Tree = Leaf Int | Node Tree Tree deriving Show

data TreeBitSet = TBS Int Tree deriving Show

bitsPerLeaf :: Int
bitsPerLeaf = 64

-- returns true if capacity is 64 * 2^n for some n >= 0
isValidCapacity :: Int -> Bool
isValidCapacity capacity
  | capacity <= 0           = False
  | capacity == bitsPerLeaf = True
  | m == 0                  = isValidCapacity half
  | otherwise               = False
  where
    (half, m) = divMod capacity 2

-- =================================================================

-- | Función para crear un árbol con la capacidad dada
makeTree :: Int -> Tree
makeTree tam
  | tam <= bitsPerLeaf = Leaf 0 
  | otherwise          = Node (makeTree half) (makeTree half)
    where 
      half = tam `div` 2

-- | Función para crear un TreeBitSet vacío con la capacidad dada
empty :: Int -> TreeBitSet
empty tam
  | tam < 0                   = error "capacity must be positive"
  | not (isValidCapacity tam) = error "capacity must be 64 multiplied by a power of 2"
  | otherwise                 = TBS tam (makeTree tam)

-- | Función para obtener el tamaño de un TreeBitSet
size :: TreeBitSet -> Int
size (TBS _ tree) = sizeRec tree 
  where 
    sizeRec (Leaf bitSet) = IntBits.countBits bitSet 
    sizeRec (Node lt rt)  = sizeRec lt + sizeRec rt

-- | Función para obtener la capacidad de un TreeBitSet
capacity :: TreeBitSet -> Int
capacity (TBS tam _) = tam

-- | Función para verificar si un TreeBitSet está vacío
isEmpty :: TreeBitSet -> Bool
isEmpty tbs = size tbs == 0

-- | Función para verificar si un elemento está fuera del rango de un TreeBitSet
outOfRange :: TreeBitSet -> Int -> Bool
outOfRange (TBS tam _) i = i < 0 || i >= tam

-- | Función para verificar si un elemento está contenido en un TreeBitSet
contains :: Int -> TreeBitSet -> Bool
contains i tbs@(TBS tam tree) 
  | outOfRange tbs i = False 
  | otherwise        = containsRec i tam tree 
    where 
      containsRec i _ (Leaf bitSet)  = IntBits.bitValue i bitSet
      containsRec i tam (Node lt rt)
        | i < half  = containsRec i half lt 
        | otherwise = containsRec (i - half) half rt
            where 
              half = tam `div` 2

-- | Función para agregar un elemento a un TreeBitSet
add :: Int -> TreeBitSet -> TreeBitSet
add i tbs@(TBS tam tree) 
  | outOfRange tbs i = error "add on tbs with index out of bounds"
  | otherwise        = TBS tam (addRec i tam tree)
    where 
      addRec i _ (Leaf bitSet)  = Leaf (IntBits.setBit' i bitSet)
      addRec i tam (Node lt rt)
        | i < half  = Node (addRec i half lt) rt
        | otherwise = Node lt (addRec (i - half) half rt)
          where 
            half = tam `div` 2 

-- | Función para convertir un TreeBitSet a una lista de elementos
toList :: TreeBitSet -> [Int]
toList (TBS tam tree) = toListRec 0 tam tree 
  where 
    toListRec offSet _ (Leaf bitSet)  = map (+offSet) (IntBits.toList bitSet)
    toListRec offSet tam (Node lt rt) = toListRec offSet half lt ++ toListRec (offSet + half) half rt 
      where 
        half = tam `div` 2

-- | Función para realizar la unión de dos TreeBitSets
union :: TreeBitSet -> TreeBitSet -> TreeBitSet
union (TBS tam1 tree1) (TBS tam2 tree2)
  | tam1 /= tam2 = error "union on TBS with different shape"
  | otherwise    = TBS tam1 (unionRec tree1 tree2)
    where 
      unionRec (Leaf bitSet1) (Leaf bitSet2) = Leaf (foldr IntBits.setBit' bitSet2 (IntBits.toList bitSet1))  
      unionRec (Node lt1 rt1) (Node lt2 rt2) = Node (unionRec lt1 lt2) (unionRec rt1 rt2)

-- | Función para realizar la unión extendida de dos TreeBitSets
extendedUnion :: TreeBitSet -> TreeBitSet -> TreeBitSet
extendedUnion tbs1@(TBS tam1 tree1) tbs2@(TBS tam2 tree2)
  | tam1 < tam2  = TBS tam2 (extendedUnionRec tam1 tree1 tam2 tree2) 
  | tam1 == tam2 = union tbs1 tbs2
  | otherwise    = TBS tam1 (extendedUnionRec tam1 tree1 tam2 tree2)
    where 
      extendedUnionRec tam1 (Leaf bitSet1) tam2 (Leaf bitSet2) = Leaf (foldr IntBits.setBit' bitSet2 (IntBits.toList bitSet1)) 
       where 
          half1 = tam1 `div` 2 
          half2 = tam2 `div` 2
      extendedUnionRec tam1 (Leaf bitSet1) tam2 (Node lt2 rt2) = Node (extendedUnionRec tam1 (Leaf bitSet1) half2 lt2) rt2
        where 
          half1 = tam1 `div` 2 
          half2 = tam2 `div` 2
      extendedUnionRec tam1 (Node lt1 rt1) tam2 (Leaf bitSet2) = Node (extendedUnionRec half1 lt1 tam2 (Leaf bitSet2)) rt1
        where 
          half1 = tam1 `div` 2 
          half2 = tam2 `div` 2
      extendedUnionRec tam1 (Node lt1 rt1) tam2 (Node lt2 rt2) = Node (extendedUnionRec half1 lt1 half2 lt2) (extendedUnionRec half1 rt1 half2 rt2)
        where 
          half1 = tam1 `div` 2 
          half2 = tam2 `div` 2


-- =========================================

instance Arbitrary TreeBitSet where
    arbitrary = do
        exponent <- chooseInt (0, 2)
        let capacity = bitsPerLeaf * 2^exponent
        elements <- listOf (chooseInt (0, capacity-1))
        return (foldr add (empty capacity) elements)
            where
                chooseInt :: (Int, Int ) -> Gen Int 
                chooseInt (x,y) = choose (x,y)        

instance Eq TreeBitSet where
  tbs1 == tbs2 = toInts tbs1 == toInts tbs2
    where
      toInts (TBS _ t) = reverse . dropWhile (==0) . reverse $ toInts' t
      toInts' (Leaf b) = [b]
      toInts' (Node lt rt) = toInts' lt ++ toInts' rt

-- =========================================


validExponent e = e >= 0 && e <= 5

validElement e (TBS c t) = e >= 0 && e < c

-- Axioms for basic set of operations
ax1 e = validExponent e ==> isEmpty emptySet
  where emptySet = empty (64*2^e)

ax2 x s = validElement x s ==> not (isEmpty (add x s))

ax3 e x = validExponent e ==> not (contains x emptySet)
  where emptySet = empty (64*2^e)

ax4 x y s = validElement y s ==> contains x (add y s) == (x==y) || contains x s

ax5 e = validExponent e ==> size emptySet == 0
  where emptySet = empty (64*2^e)

ax6 x s = contains x s  ==> size (add x s) == size s
ax7 x s = validElement x s && not (contains x s) ==> size (add x s) == 1 + size s

ax11 e = validExponent e ==> null (toList emptySet)
  where emptySet = empty (64*2^e)

ax12 x s = contains x s  ==> toList (add x s) == toList s

ax13 x s = validElement x s && not (contains x s) ==> x `elem` xs && (xs \\ [x]) `sameElements` toList s
  where
    xs = toList (add x s)
    sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)


type Elem = Int

setAxioms = do
  quickCheck (ax1 :: Int -> Property)
  quickCheck (ax2 :: Elem -> TreeBitSet -> Property)
  quickCheck (ax3 :: Int -> Elem -> Property)
  quickCheck (ax4 :: Elem -> Elem -> TreeBitSet -> Property)
  quickCheck (ax5 :: Int -> Property)
  quickCheck (ax6 :: Elem -> TreeBitSet -> Property)
  quickCheck (ax7 :: Elem -> TreeBitSet -> Property)
  quickCheck (ax11 :: Int -> Property)
  quickCheck (ax12 :: Elem -> TreeBitSet -> Property)
  quickCheck (ax13 :: Elem -> TreeBitSet -> Property)
