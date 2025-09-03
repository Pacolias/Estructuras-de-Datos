-------------------------------------------------------------------------------
-- Student's name: Paco Molina Cuenca
-- Student's group:
-- Identity number (DNI if Spanish/passport if Erasmus): 
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Trie.DictionaryStringTrie(
    Trie()
  , empty
  , isEmpty
  , size
  , search
  , insert
  , strings
  , fromList
  , pretty
  , sampleTrie, sampleTrie1, sampleTrie2, sampleTrie3, sampleTrie4
  -- sizeValue, toTrie, childOf, update
  ) where

import qualified Control.DeepSeq as Deep
import Data.Maybe
import qualified DataStructures.Dictionary.AVLDictionary as D

data Trie a = Empty | Node (Maybe a) (D.Dictionary Char (Trie a)) deriving Show

-------------------------------------------------------------------------------
-- DO NOT WRITE ANY CODE ABOVE ------------------------------------------------
-------------------------------------------------------------------------------

-- | = Exercise a - empty
empty :: Trie a
empty = Empty

-- | = Exercise b - isEmpty
isEmpty :: Trie a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | = Exercise c - sizeValue
sizeValue :: Maybe a -> Int
sizeValue (Just _) = 1
sizeValue Nothing = 0

-- | = Exercise d - size
size :: Trie a -> Int
size Empty = 0
size (Node valor diccionario) = sizeValue valor + sum[size t | t <- D.values diccionario]

-- | = Exercise e - toTrie
toTrie :: Maybe (Trie a) -> Trie a
toTrie (Just t) = t
toTrie Nothing = Empty

-- | = Exercise f - childOf
childOf :: Char -> Trie a -> Trie a
childOf _ Empty = Empty
childOf a (Node _ diccionario) = 
  case (D.valueOf a diccionario) of
    Nothing   -> Empty
    Just trie -> trie

-- | = Exercise g - search
search :: String -> Trie a -> Maybe a
search _ Empty = Nothing
search [] (Node valor _) = valor
search (c:cs) t = search cs (childOf c t)

-- | = Exercise h - update
update :: Trie a -> Char -> Trie a -> Trie a
update Empty valor child = Node Nothing (D.insert valor child D.empty)
update (Node val diccionario) valor child = Node val (D.updateOrInsert valor (const child) child diccionario)

-- | = Exercise i - insert
insert :: String -> a -> Trie a -> Trie a
insert [] v Empty = Node (Just v) D.empty
insert [] v (Node valor diccionario) = Node (Just v) diccionario
insert (c:cs) v trie = update trie c updatedChild
  where
    updatedChild = insert cs v (childOf c trie)

-- Inserta las palabras "to" (1), "tea" (2), "ten" (3) en un trie vacío
exampleTrie :: Trie Int
exampleTrie = foldl (\trie (w,v) -> insert w v trie) Empty
  [("to", 0), ("tea", 1), ("ten", 2)]

main :: IO ()
main = print exampleTrie

-------------------------------------------------------------------------------
-- ONLY FOR PART TIME STUDENTS ------------------------------------------------
-------------------------------------------------------------------------------

-- | = Exercise e1 - strings
strings :: Trie a -> [String]
strings Empty = []
strings trie = aniadirLetra trie ""
  where 
    aniadirLetra :: Trie a -> String -> [String]
    aniadirLetra (Node valor diccionario) prefijo = 
      let actual = [prefijo | isJust valor]
          children = concat [aniadirLetra (childOf k (Node valor diccionario)) (prefijo ++ [k]) | k <- D.keys diccionario]
      in actual ++ children

exampleTrieStrings = foldl (\t (w, v) -> insert w v t) Empty [("to", 1), ("tea", 2), ("ten", 3)]
mainStrings = print (strings exampleTrieStrings)


-- | = Exercise e2 - fromList
fromList :: [String] -> Trie Int
fromList [] = Empty 
fromList xs = aniadir xs 0
  where 
    aniadir :: [String] -> Int -> Trie Int 
    aniadir [] _ = Empty
    aniadir (x:xs) acc = insert x acc (aniadir xs (acc+1))

exampleFromList = ["to", "tea", "ten"]
mainFromList = print (fromList exampleFromList)

-------------------------------------------------------------------------------
-- DO NOT WRITE ANY CODE BELOW ------------------------------------------------
-------------------------------------------------------------------------------

pretty :: (Show a) => Trie a -> IO ()
pretty t = putStrLn (showsTrie t "")

showsTrie :: (Show a) => Trie a -> ShowS
showsTrie Empty       = shows "Empty"
showsTrie (Node mb d) = showString "Node " . showValue mb . showChar '\n' . aux 1 d
  where
    aux n d =
      foldr (.) id [ showString (replicate (6*n) ' ')
                      . showChar c
                      . showString " -> "
                      . showString "Node "
                      . showValue mb
                      . showChar '\n'
                      . aux (n+1) d'
                    | (c, Node mb d') <- D.keysValues d
                    ]

    showValue mb = maybe (shows mb) (const (showChar '(' . shows mb . showChar ')')) mb

instance (Eq a) => Eq (Trie a) where
  Empty     == Empty       = True
  Node mb d == Node mb' d' = mb == mb' && d == d'
  _         == _           = False

instance (Deep.NFData a) => Deep.NFData (Trie a) where
  rnf Empty       = ()
  rnf (Node mb d) = mb `Deep.deepseq` rnfDict d
    where
      rnfDict = D.foldKeysValues (\k v d -> k `Deep.deepseq` v `Deep.deepseq` v `Deep.deepseq` d) ()


sampleTrie :: Trie Integer
sampleTrie = n0
   -- bat -> 0  be -> 1  bed -> 2  cat -> 3  to -> 4  toe -> 5
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('b', n1), ('c', n6), ('t', n9)]
      n1 = Node Nothing $ children [('a', n2), ('e', n4)]
      n2 = Node Nothing $ children [('t', n3)]
      n3 = Node (Just 0) $ children []
      n4 = Node (Just 1) $ children [('d', n5)]
      n5 = Node (Just 2) $ children []
      n6 = Node Nothing $ children [('a', n7)]
      n7 = Node Nothing $ children [('t', n8)]
      n8 = Node (Just 3) $ children []
      n9 = Node Nothing $ children [('o', n10)]
      n10 = Node (Just 4) $ children [('e', n11)]
      n11 = Node (Just 5) $ children []

sampleTrie1 :: Trie Integer
sampleTrie1 = n0
   -- a -> 3  b -> 2  c -> 1
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1), ('b', n2), ('c', n3)]
      n1 = Node (Just 3) $ children []
      n2 = Node (Just 2) $ children []
      n3 = Node (Just 1) $ children []

sampleTrie2 :: Trie Integer
sampleTrie2 = n0
   -- a -> 1  ab -> 2  abc -> 3  abd -> 4  acdef -> 5
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1)]
      n1 = Node (Just 1) $ children [('b', n2), ('c', n5)]
      n2 = Node (Just 2) $ children [('c', n3), ('d', n4)]
      n3 = Node (Just 3) $ children []
      n4 = Node (Just 4) $ children []
      n5 = Node Nothing $ children [('d', n6)]
      n6 = Node Nothing $ children [('e', n7)]
      n7 = Node Nothing $ children [('f', n8)]
      n8 = Node (Just 5) $ children []

sampleTrie3 :: Trie Integer
sampleTrie3 = n0
   -- abcd -> 1
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1)]
      n1 = Node Nothing $ children [('b', n2)]
      n2 = Node Nothing $ children [('c', n3)]
      n3 = Node Nothing $ children [('d', n4)]
      n4 = Node (Just 1) $ children []

sampleTrie4 :: Trie Integer
sampleTrie4 = n0
   -- abcd -> 1  def -> 2
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1), ('d', n5)]
      n1 = Node Nothing $ children [('b', n2)]
      n2 = Node Nothing $ children [('c', n3)]
      n3 = Node Nothing $ children [('d', n4)]
      n4 = Node (Just 1) $ children []
      n5 = Node Nothing $ children [('e', n6)]
      n6 = Node Nothing $ children [('f', n7)]
      n7 = Node (Just 2) $ children []
