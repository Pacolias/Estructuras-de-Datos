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
  | tam <= bitsPerLeaf  = Leaf 0
  | otherwise           = Node (makeTree (tam `div` 2)) (makeTree (tam `div` 2))

-- | Función para crear un TreeBitSet vacío con la capacidad dada
empty :: Int -> TreeBitSet
empty n
  | not(isValidCapacity n)  = if n < 0 then error "capacity must be positive" else error "capacity must be 64 multiplied by a power of 2"
  | otherwise               = TBS n (makeTree n)

-- | Función para obtener el tamaño de un TreeBitSet
size :: TreeBitSet -> Int
size (TBS tam tree) = sizeTree tree
  where 
    sizeTree :: Tree -> Int
    sizeTree (Leaf bitSet)  = IntBits.countBits bitSet 
    sizeTree (Node lt rt)   = sizeTree lt + sizeTree rt

-- | Función para obtener la capacidad de un TreeBitSet
capacity :: TreeBitSet -> Int
capacity (TBS tam _) = tam

-- | Función para verificar si un TreeBitSet está vacío
isEmpty :: TreeBitSet -> Bool
isEmpty tbs 
  | size tbs == 0 = True 
  | otherwise     = False

-- | Función para verificar si un elemento está fuera del rango de un TreeBitSet
outOfRange :: TreeBitSet -> Int -> Bool
outOfRange tbs num
  | num < 0 || num >= capacity tbs  = True
  | otherwise                       = False

-- | Función para verificar si un elemento está contenido en un TreeBitSet
contains :: Int -> TreeBitSet -> Bool
contains elem tbs@(TBS tam tree)
  | outOfRange tbs elem = False 
  | otherwise           = containsTree elem tree
    where 
      containsTree :: Int -> Tree -> Bool 
      containsTree element (Leaf bitSet)  = IntBits.bitValue element bitSet 
      containsTree element (Node lt rt)
        | element < tam `div` 2 = containsTree element lt
        | otherwise             = containsTree (element - tam `div` 2) rt

      -- 0 <= elem <= tam - 1 
      -- 0 <= elem < tam/2
      -- tam/2 <= elem <= tam - 1 ==> 0 <= elem - tam/2 <= tam/2 - 1

-- | Función para agregar un elemento a un TreeBitSet
add :: Int -> TreeBitSet -> TreeBitSet
add elem tbs@(TBS tam tree)
  | outOfRange tbs elem = error "The element is out of range for the given tree"
  | contains elem tbs   = tbs
  | otherwise           = TBS tam (addTree elem tree)
    where 
      addTree :: Int -> Tree -> Tree 
      addTree element (Leaf bitSet) = Leaf (IntBits.setBit' element bitSet) 
      addTree element (Node lt rt)
        | element < tam `div` 2 = Node (addTree element lt) rt
        | otherwise             = Node lt (addTree (element - tam `div` 2) rt)

-- | Función para convertir un TreeBitSet a una lista de elementos
toList :: TreeBitSet -> [Int]
toList tbs@(TBS tam tree) = toListTree 0 tam tree 
  where 
    toListTree :: Int -> Int -> Tree -> [Int]
    toListTree offSet _ (Leaf bitSet)       = map (+offSet) (IntBits.toList bitSet)
    toListTree offSet tamanio (Node lt rt)  = toListTree offSet (tamanio `div` 2) lt ++ toListTree (offSet + tamanio `div` 2) (tamanio `div` 2) rt

-- | Función para realizar la unión de dos TreeBitSets
union :: TreeBitSet -> TreeBitSet -> TreeBitSet
union tbs1 tbs2 = foldr add tbs1 (toList tbs2)

-- | Función para realizar la unión extendida de dos TreeBitSets
extendedUnion :: TreeBitSet -> TreeBitSet -> TreeBitSet
extendedUnion tbs1 tbs2 
  | capacity tbs1 <= capacity tbs2  = foldr add tbs2 (toList tbs1)
  | otherwise                       = foldr add tbs1 (toList tbs2)

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
