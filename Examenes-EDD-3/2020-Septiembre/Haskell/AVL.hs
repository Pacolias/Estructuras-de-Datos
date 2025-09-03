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
  | capacity < w = error "addObject on bin with not enough remaining capacity" 
  | otherwise    = B (capacity - w) (w:ws)

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty                   = 0
maxRemainingCapacity (Node _ _ capacity _ _) = capacity

height :: AVL -> Int
height Empty = 0 
height (Node _ h _ _ _) = h

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight bin h lt rt = Node bin h (maximum [remainingCapacity bin, maxRemainingCapacity lt, maxRemainingCapacity rt]) lt rt 

node :: Bin -> AVL -> AVL -> AVL
node bin lt rt = nodeWithHeight bin (1 + max (height lt) (height rt)) lt rt

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c lt (Node x _ _ r1 r2) = node x (node c lt r1) r2

addNewBin :: Bin -> AVL -> AVL
addNewBin c Empty                 = Node c 1 (remainingCapacity c) Empty Empty
addNewBin c (Node x _ _ lt rt)
  | height child' > height lt + 1 = rotateLeft x lt child' 
  | otherwise                     = node x lt child'
    where 
      child' = addNewBin c rt
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst capacity w Empty        = addNewBin (addObject w (emptyBin capacity)) Empty
addFirst capacity w avl@(Node bin h cap lt rt)
  | maxRemainingCapacity avl < w = addNewBin (addObject w (emptyBin capacity)) avl 
  | maxRemainingCapacity lt >= w = node bin (addFirst capacity w lt) rt
  | remainingCapacity bin >= w   = node (addObject w bin) lt rt
  | otherwise                    = node bin lt (addFirst capacity w rt)

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
linearBinPacking capacity = foldr addBin SEmpty
  where 
    addBin :: Weight -> Sequence -> Sequence 
    addBin w SEmpty = SNode (addObject w (emptyBin capacity)) SEmpty
    addBin w (SNode bin next)
      | remainingCapacity bin >= w = SNode (addObject w bin) next 
      | otherwise                  = SNode bin (addBin w next)

seqToList:: Sequence -> [Bin]
seqToList SEmpty = []
seqToList (SNode bin next) = bin : seqToList next

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold ws capacity = foldr (addFirst capacity) Empty ws

{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"
