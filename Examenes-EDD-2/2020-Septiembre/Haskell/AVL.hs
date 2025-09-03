 {------------------------------------------------------------------------------
 - Student's name: Paco Molina Cuenca
 -
 - Student's group: Doble Grado Matemáticas e Ingeniería Informática.
 -----------------------------------------------------------------------------}

module AVL 
  ( 
    Weight
  , Capacity
  , AVL (..)
  , Bin
  , emptyBin
  , remainingCapacity
  , addObject
  , maxRemainingCapacity
  , height
  , nodeWithHeight
  , node
  , rotateLeft
  , addNewBin
  , addFirst
  , addAll
  , toList
  , linearBinPacking
  , seqToList
  , addAllFold
  ) where

type Capacity = Int
type Weight= Int

data Bin = B Capacity [Weight] 

data AVL = Empty | Node Bin Int Capacity AVL AVL deriving Show


emptyBin :: Capacity -> Bin
emptyBin capacity = B capacity []

remainingCapacity :: Bin -> Capacity
remainingCapacity (B capacity _) = capacity

addObject :: Weight -> Bin -> Bin
addObject w (B capacity ws)
  | w > capacity  = error "Too heavy object for the given bin"
  | otherwise     = B (capacity - w) (w:ws) 

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty                    = 0
maxRemainingCapacity (Node _ _ capacity _ _)  = capacity

height :: AVL -> Int
height Empty            = 0
height (Node _ h _ _ _) = h

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight bin height lt rt = Node bin height (maximum [remainingCapacity bin, maxRemainingCapacity lt, maxRemainingCapacity rt]) lt rt

node :: Bin -> AVL -> AVL -> AVL
node bin lt rt = nodeWithHeight bin (1 + max (height lt) (height rt)) lt rt

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c l (Node x _ _ r1 r2) = node x (node c l r1) r2  

addNewBin :: Bin -> AVL -> AVL
addNewBin bin Empty = Node bin 1 (remainingCapacity bin) Empty Empty
addNewBin bin (Node x h maxCapacity lt rt)
  | height rightChild > height lt + 1 = rotateLeft x lt rightChild
  | otherwise                         = node x lt rightChild
  where 
    rightChild = addNewBin bin rt

addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst capacity w Empty         = Node (B (capacity - w) [w]) 1 (capacity - w) Empty Empty
addFirst capacity w (Node bin height maxCapacity lt rt)
  | maxRemainingCapacity lt >= w  = node bin (addFirst capacity w lt) rt 
  | remainingCapacity bin >= w    = node (addObject w bin) lt rt
  | maxRemainingCapacity rt >= w  = node bin lt (addFirst capacity w rt)
  | otherwise                     = node bin lt (addNewBin (B (capacity - w) [w]) rt)

addAll:: Capacity -> [Weight] -> AVL
addAll capacity = foldr (addFirst capacity) Empty 

toList :: AVL -> [Bin]
toList Empty = []
toList (Node bin _ _ lt rt) = toList lt ++ [bin] ++ toList rt 

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
-}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking capacity = foldl insert SEmpty
  where 
    insert :: Sequence -> Weight -> Sequence 
    insert SEmpty w                   = SNode (B (capacity - w) [w]) SEmpty
    insert (SNode bin seq) w 
      | remainingCapacity bin >= w    = SNode (addObject w bin) seq
      | otherwise                     = SNode bin (insert seq w)

seqToList:: Sequence -> [Bin]
seqToList SEmpty          = []
seqToList (SNode bin seq) = bin : seqToList seq

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold weights capacity = foldr (addFirst capacity) Empty weights 

{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"
