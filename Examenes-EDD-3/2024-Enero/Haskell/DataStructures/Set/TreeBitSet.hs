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
  | otherwise          = Node (makeTree (tam `div` 2)) (makeTree (tam `div` 2))

-- | Función para crear un TreeBitSet vacío con la capacidad dada
empty :: Int -> TreeBitSet
empty tam 
  | not(isValidCapacity tam) && tam < 0 = error "capacity must be positive"
  | not(isValidCapacity tam)            = error "capacity must be 64 multiplied by a power of 2"
  | otherwise                           = TBS tam (makeTree tam)

-- | Función para obtener el tamaño de un TreeBitSet
size :: TreeBitSet -> Int
size (TBS tam tree) = sizeRec tree 
  where
    sizeRec :: Tree -> Int
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
contains elem tbs@(TBS tam tree) 
  | isEmpty tbs || outOfRange tbs elem = False 
  | otherwise                          = containsRec elem tam tree 
    where 
      containsRec :: Int -> Int -> Tree -> Bool
      containsRec i _ (Leaf bitSet) = IntBits.bitValue i bitSet 
      containsRec i tam (Node lt rt)
        | i < tam `div` 2 = containsRec i (tam `div` 2) lt
        | otherwise       = containsRec (i - (tam `div` 2)) (tam `div` 2) rt

-- | Función para agregar un elemento a un TreeBitSet
add :: Int -> TreeBitSet -> TreeBitSet
add elem tbs@(TBS tam tree)
  | outOfRange tbs elem = error "element is out of range" 
  | contains elem tbs   = tbs 
  | otherwise           = TBS tam (addRec elem tam tree)
    where 
      addRec :: Int -> Int -> Tree -> Tree 
      addRec i _ (Leaf bitSet)  = Leaf (IntBits.setBit' i bitSet)
      addRec i tam (Node lt rt)
        | i < tam `div` 2 = Node (addRec i (tam `div` 2) lt) rt
        | otherwise       = Node lt (addRec (i - (tam `div` 2)) (tam `div` 2) rt)   

-- | Función para convertir un TreeBitSet a una lista de elementos
toList :: TreeBitSet -> [Int]
toList (TBS tam tree) = toListRec tree 0
  where 
    toListRec :: Tree -> Int -> [Int]
    toListRec (Leaf bitSet) _ = IntBits.toList bitSet 
    toListRec (Node lt rt) offSet = toListRec lt offSet ++ toListRec rt (offSet + bitsPerLeaf)

-- | Función para realizar la unión de dos TreeBitSets
union :: TreeBitSet -> TreeBitSet -> TreeBitSet
union tbs1@(TBS tam1 tree1) tbs2@(TBS tam2 tree2)
  | tam1 /= tam2 = error "sets have different capacities"
  | otherwise    = foldr add tbs1 (toList tbs2)

-- | Función para realizar la unión extendida de dos TreeBitSets
extendedUnion :: TreeBitSet -> TreeBitSet -> TreeBitSet
extendedUnion tbs1 tbs2 = foldr add tbs1 (toList tbs2)


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
