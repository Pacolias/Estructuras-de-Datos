module Main where

import DataStructures.Trie.DictionaryStringTrie
import qualified DataStructures.Dictionary.AVLDictionary as D

main :: IO ()
main = do
  putStrLn "== Test de exampleTrie (insert y pretty):"
  pretty exampleTrie

  putStrLn "\n== Test de strings (debería mostrar: [\"tea\",\"ten\",\"to\"]):"
  print (strings exampleTrieStrings)

  putStrLn "\n== Test de search:"
  print (search "tea" exampleTrieStrings)   -- Just 2
  print (search "ten" exampleTrieStrings)   -- Just 3
  print (search "to" exampleTrieStrings)    -- Just 1
  print (search "te" exampleTrieStrings)    -- Nothing
  print (search "toast" exampleTrieStrings) -- Nothing

  putStrLn "\n== Test de fromList + strings:"
  let wordsList = ["cat", "car", "cab"]
  let trieFromList = fromList wordsList
  print (strings trieFromList) -- ["cab", "car", "cat"]

  putStrLn "\n== Test de size (exampleTrie tiene 3 palabras):"
  print (size exampleTrieStrings) -- 3

  putStrLn "\n== Test de isEmpty:"
  print (isEmpty empty)         -- True
  print (isEmpty exampleTrie)   -- False

  putStrLn "\n== Test de sampleTrie:"
  pretty sampleTrie
  putStrLn "\nPalabras en sampleTrie:"
  print (strings sampleTrie)

---------------------------------------------------------------
exampleTrie :: Trie Int
exampleTrie = foldl (\trie (w,v) -> insert w v trie) empty
  [("to", 1), ("tea", 2), ("ten", 3)]

exampleTrieStrings :: Trie Int
exampleTrieStrings = exampleTrie
