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
makeTree n
  | n <= bitsPerLeaf = Leaf 0
  | otherwise = Node (makeTree mitad) (makeTree mitad)
    where mitad = div n 2


-- | Función para crear un TreeBitSet vacío con la capacidad dada
empty :: Int -> TreeBitSet
empty n 
  | isValidCapacity n = (TBS n (makeTree n))
  | otherwise = if n < 0 then error "capacity must be positive" else error "capacity must be 64 multiplied by a power of 2"


-- | Función para obtener el tamaño de un TreeBitSet
size :: TreeBitSet -> Int
size (TBS _ tree) = contar tree
  where
    contar :: Tree -> Int
    contar (Leaf bits)     = IntBits.countBits bits
    contar (Node left right) = contar left + contar right


-- | Función para obtener la capacidad de un TreeBitSet
capacity :: TreeBitSet -> Int
capacity (TBS n _) = n


-- | Función para verificar si un TreeBitSet está vacío
isEmpty :: TreeBitSet -> Bool
isEmpty tbs = size tbs == 0


-- | Función para verificar si un elemento está fuera del rango de un TreeBitSet
outOfRange :: TreeBitSet -> Int -> Bool
outOfRange (TBS capacidad _) n = n < 0 || n >= capacidad

-- | Función para verificar si un elemento está contenido en un TreeBitSet
contains :: Int -> TreeBitSet -> Bool
contains n (TBS c tree)
  | outOfRange (TBS c tree) n = False 
  | otherwise = busca n c tree 
    where
      busca :: Int -> Int -> Tree -> Bool
      busca x c (Leaf bits) = IntBits.bitValue x bits
      busca x c (Node lt rt)
        | x < mitad = busca x mitad lt
        | otherwise = busca (x - mitad) mitad rt
          where mitad = div c 2 


-- | Función para agregar un elemento a un TreeBitSet
add :: Int -> TreeBitSet -> TreeBitSet
add n (TBS c tree)
  | outOfRange (TBS c tree) n = error "element is out of range"
  | otherwise = (TBS c (inserta n c tree))
    where
      inserta :: Int -> Int -> Tree -> Tree
      inserta n c (Leaf bits) = Leaf (IntBits.setBit' n bits) 
      inserta n c (Node lt rt)
        | n < mitad = Node (inserta n mitad lt) rt
        | otherwise = Node lt (inserta (n - mitad) mitad rt)
          where mitad = div c 2 


-- | Función para convertir un TreeBitSet a una lista de elementos
toList :: TreeBitSet -> [Int]
toList (TBS c tree) = listar 0 c tree 
  where
    listar :: Int -> Int -> Tree -> [Int]
    listar offset _ (Leaf bits) = map (+ offset) (IntBits.toList bits) 
    listar offset c (Node lt rt) = (listar offset mitad lt) ++ (listar (offset + mitad) mitad rt)
      where mitad = div c 2

--000100 2

--toList' derecho mitad
--Se llama recursivamente la función toList' en el subárbol derecho con la mitad de la capacidad como argumento. 
--Esto genera una lista de elementos contenidos en el subárbol derecho.

--map (+mitad) ...
--Se aplica la función (+mitad) a cada elemento de la lista generada en el paso anterior. Esto significa que se suma la mitad de la capacidad a cada elemento de la lista.


-- | Función para realizar la unión de dos TreeBitSets
union :: TreeBitSet -> TreeBitSet -> TreeBitSet
union = undefined


-- | Función para realizar la unión extendida de dos TreeBitSets
extendedUnion :: TreeBitSet -> TreeBitSet -> TreeBitSet
extendedUnion = undefined


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