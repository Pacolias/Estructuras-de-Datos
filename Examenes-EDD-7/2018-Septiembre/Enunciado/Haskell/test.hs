-------------------------------------------------------------------------------
-- Pruebas HUnit para AVLBiDictionary
-------------------------------------------------------------------------------

module Main where

import Test.HUnit
import qualified DataStructures.Dictionary.AVLDictionary as D
import AVLBiDictionary

-- Función auxiliar: construir un BiDictionary a partir de una lista de pares
fromListBD :: (Ord a, Ord b) => [(a,b)] -> BiDictionary a b
fromListBD = foldl (flip (uncurry insert)) empty

-- Casos de prueba
tests :: Test
tests = TestList
  [ "empty_isEmpty" ~: True ~=? isEmpty (empty :: BiDictionary Int String)
  , "empty_size"    ~: 0    ~=? size (empty :: BiDictionary Int String)

  , "insert_size" ~:
      3 ~=? size (fromListBD [(1,"a"), (2,"b"), (3,"c")])

  , "insert_override" ~:
      Just "z" ~=? valueOf 1 (fromListBD [(1,"x"), (1,"z")])

  , "keyOf_existing" ~:
      Just 1 ~=? keyOf "a" (fromListBD [(1,"a"), (2,"b")])

  , "keyOf_missing" ~:
      Nothing ~=? keyOf "x" (fromListBD [(1,"a")])

  , "valueOf_existing" ~:
      Just "b" ~=? valueOf 2 (fromListBD [(1,"a"), (2,"b")])

  , "deleteByKey_existing" ~:
      Nothing ~=? valueOf 2 (deleteByKey 2 (fromListBD [(1,"a"), (2,"b")]))

  , "deleteByKey_missing" ~:
      1 ~=? size (deleteByKey 99 (fromListBD [(1,"a")]))

  , "deleteByValue_existing" ~:
      Nothing ~=? keyOf "b" (deleteByValue "b" (fromListBD [(1,"a"), (2,"b")]))

  , "deleteByValue_missing" ~:
      1 ~=? size (deleteByValue "z" (fromListBD [(1,"a")]))

  , "toBiDictionary_injective" ~:
      2 ~=? size (toBiDictionary (D.insert 1 "a" (D.insert 2 "b" D.empty)))

  , "compose_simple" ~:
      Just 'x' ~=? valueOf 1 (compose (fromListBD [(1,"a")]) (fromListBD [("a",'x')]))

  , "isPermutation_true" ~:
      True ~=? isPermutation (fromListBD [(1,2),(2,1)])

  , "isPermutation_false" ~:
      False ~=? isPermutation (fromListBD [(1,2),(2,2)])

  , "orbitOf_cycle" ~:
      [2,1] ~=? orbitOf 1 (fromListBD [(1,2),(2,1)])

  , "cyclesOf_twoCycles" ~:
      [[2,1],[3]] ~=? cyclesOf (fromListBD [(1,2),(2,1),(3,3)])
  ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

