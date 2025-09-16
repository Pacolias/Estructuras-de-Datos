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
  | w > capacity = error "addObject on bin with not enough capacity"
  | otherwise    = B (capacity - w) (w:ws)

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0 
maxRemainingCapacity (Node _ _ capacity _ _) = capacity

height :: AVL -> Int
height Empty            = 0
height (Node _ h _ _ _) = h

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight bin h lt rt = Node bin h (maximum [remainingCapacity bin, maxRemainingCapacity lt, maxRemainingCapacity rt]) lt rt

node :: Bin -> AVL -> AVL -> AVL
node bin lt rt = nodeWithHeight bin (max (height lt) (height rt)) lt rt 

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft bin l (Node x _ _ r1 r2) = node x (node bin l r1) r2

addNewBin :: Bin -> AVL -> AVL
addNewBin bin Empty = node bin Empty Empty 
addNewBin bin (Node x h capacity lt rt)
  | height rt' > height lt + 1 = rotateLeft x lt rt' 
  | otherwise                  = node x lt rt' 
  where 
    rt' = addNewBin bin rt
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst capacity w Empty = addNewBin (addObject w (emptyBin capacity)) Empty 
addFirst capacity w avl@(Node x _ _ lt rt)
  | maxRemainingCapacity avl < w = addNewBin (addObject w (emptyBin capacity)) avl
  | maxRemainingCapacity lt >= w = node x (addFirst capacity w lt) rt 
  | remainingCapacity x >= w     = node (addObject w x) lt rt 
  | otherwise                    = node x lt (addFirst capacity w rt) 

addAll:: Capacity -> [Weight] -> AVL
addAll capacity = foldr (addFirst capacity) Empty

toList :: AVL -> [Bin]
toList Empty = []
toList (Node x _ _ lt rt) = toList lt ++ [x] ++ toList rt

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
-}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking capacity = foldr insert SEmpty 
  where 
    insert w SEmpty = SNode (addObject w (emptyBin capacity)) SEmpty
    insert w (SNode bin seq)
      | remainingCapacity bin >= w = SNode (addObject w bin) seq 
      | otherwise                  = SNode bin (insert w seq)

seqToList:: Sequence -> [Bin]
seqToList SEmpty          = []
seqToList (SNode bin seq) = bin : seqToList seq

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold ws capacity = foldr (addFirst capacity) Empty ws



{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"
