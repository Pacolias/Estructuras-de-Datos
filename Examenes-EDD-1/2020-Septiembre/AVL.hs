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
emptyBin capacidad = B capacidad []

remainingCapacity :: Bin -> Capacity
remainingCapacity (B capacidad _) = capacidad

addObject :: Weight -> Bin -> Bin
addObject peso (B capacidad xs)
  | capacidad < peso = error "El cubo tiene capacidad suficiente para introducir el objeto dado"
  | otherwise = B (capacidad - peso) (peso : xs)

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node _ _ capacidadMax _ _ ) = capacidadMax

height :: AVL -> Int
height Empty = 0
height (Node _ altura _ _ _) = altura

nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight cubo@(B capacidad _) altura lt rt = Node cubo altura capacidadMax lt rt
  where
    capacidadMax = maximum [capacidad, maxRemainingCapacity lt, maxRemainingCapacity rt]

node :: Bin -> AVL -> AVL -> AVL
node cubo@(B capacidad _) lt rt = nodeWithHeight cubo altura lt rt
  where 
    altura = 1 + maximum [height lt, height rt]

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c arbolIzquierda Empty = error "rotateLeft: hijo derecho no puede ser Empty"
rotateLeft c arbolIzquierda (Node cubo altura capacidad r1 r2) = node cubo (node c arbolIzquierda r1) r2

addNewBin :: Bin -> AVL -> AVL
addNewBin cubo@(B capacidad _) Empty = Node cubo 1 capacidad Empty Empty 
addNewBin cubo (Node cuboRaiz altura capacidad lt rt)
  | height rt' > height lt + 1 = rotateLeft cuboRaiz lt rt' 
  | otherwise = node cuboRaiz lt rt' 
    where
      rt' = addNewBin cubo rt 
 
addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst capacidad peso Empty = addNewBin (B (capacidad - peso) [peso]) Empty 
addFirst capacidad peso (Node cubo altura capacidadMax lt rt)
  | maxRemainingCapacity lt >= peso = node cubo (addFirst capacidad peso lt) rt
  | remainingCapacity cubo >= peso = node (addObject peso cubo) lt rt
  | maxRemainingCapacity rt >= peso = node cubo lt (addFirst capacidad peso rt)
  | otherwise = node cubo lt (addNewBin (B (capacidad - peso) [peso]) rt)

addAll:: Capacity -> [Weight] -> AVL
addAll _ [] = Empty 
addAll capacidad (w:ws) = addFirst capacidad w (addAll capacidad ws) 

toList :: AVL -> [Bin]
toList Empty = []
toList (Node cubo altura capacidad lt rt) = toList lt ++ [cubo] ++ toList rt

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
-}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking capacidad lista = addLista lista SEmpty 
  where 
    addLista :: [Weight] -> Sequence -> Sequence
    addLista [] secuencia = secuencia 
    addLista (x:xs) secuencia = addLista xs (insertar x secuencia)

    insertar :: Weight -> Sequence -> Sequence 
    insertar x SEmpty = SNode (B (capacidad - x) [x]) SEmpty
    insertar x (SNode cubo secuencia)
      | remainingCapacity cubo >= x = SNode (addObject x cubo) secuencia
      | otherwise = SNode cubo (insertar x secuencia)

seqToList:: Sequence -> [Bin]
seqToList SEmpty = []
seqToList (SNode cubo secuencia) = cubo : seqToList secuencia

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold listaPesos capacidad = foldl addFirst' Empty listaPesos
  where 
    addFirst' :: AVL -> Weight -> AVL 
    addFirst' avl w = addFirst capacidad w avl

{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"
