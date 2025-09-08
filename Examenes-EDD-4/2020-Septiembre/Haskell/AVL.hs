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
  | capacity < w = error "addObject on bin with not enough capacity"
  | otherwise    = B (capacity - w) (w:ws)

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty                   = 0 
maxRemainingCapacity (Node _ _ capacity _ _) = capacity

height :: AVL -> Int
height Empty            = 0
height (Node _ h _ _ _) = h

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight bin h lt rt = Node bin h (maximum [remainingCapacity bin, maxRemainingCapacity lt, maxRemainingCapacity rt]) lt rt

node :: Bin -> AVL -> AVL -> AVL
node bin lt rt = nodeWithHeight bin (1 + max (height lt) (height rt)) lt rt

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c l r@(Node x _ _ r1 r2) = node x (node c l r1) r2

addNewBin :: Bin -> AVL -> AVL
addNewBin c Empty              = node c Empty Empty
addNewBin c (Node x h capacity lt rt)
  | height rt' > height lt + 1 = rotateLeft c lt rt'
  | otherwise                  = node x lt rt'
    where 
      rt' = addNewBin c rt
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst capacity w Empty        = addNewBin (addObject w (emptyBin capacity)) Empty
addFirst capacity w avl@(Node x _ _ lt rt)
  | maxRemainingCapacity avl < w = addNewBin (addObject w (emptyBin capacity)) avl 
  | maxRemainingCapacity lt >= w = node x (addFirst capacity w lt) rt 
  | remainingCapacity x >= w     = node (addObject w x) lt rt 
  | otherwise                    = node x lt (addFirst capacity w rt)

addAll:: Capacity -> [Weight] -> AVL
addAll capacity = foldr (addFirst capacity) Empty

toList :: AVL -> [Bin]
toList Empty              = []
toList (Node x _ _ lt rt) = toList lt ++ [x] ++ toList rt

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
-}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking capacity = foldr linearBinPackingRec SEmpty 
  where 
    linearBinPackingRec w SEmpty = SNode (addObject w (emptyBin capacity)) SEmpty
    linearBinPackingRec w (SNode bin seq)
      | remainingCapacity bin >= w = SNode (addObject w bin) seq
      | otherwise                  = SNode bin (linearBinPackingRec w seq)

seqToList:: Sequence -> [Bin]
seqToList = seqToListRec 
  where 
    seqToListRec SEmpty = []
    seqToListRec (SNode bin seq) = bin : seqToListRec seq 

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold ws capacity = foldr (addFirst capacity) Empty ws



{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"
