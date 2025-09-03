{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.HUnit
import Control.Exception (catch, SomeException)
import DataStructures.Set.TreeBitSet

tests :: Test
tests = TestList
  [ "empty should be empty" ~:
      isEmpty (empty 64) ~?= True

  , "size of empty set is 0" ~:
      size (empty 64) ~?= 0

  , "add an element increases size by 1" ~:
      size (add 5 (empty 64)) ~?= 1

  , "add the same element twice doesn't change size" ~:
      let s = add 7 (empty 64)
      in size (add 7 s) ~?= 1

  , "contains should return True for added element" ~:
      contains 8 (add 8 (empty 64)) ~?= True

  , "contains should return False for element not added" ~:
      contains 3 (empty 64) ~?= False

  , "toList should contain added elements" ~:
      let s = add 3 . add 5 . add 7 $ empty 64
      in toList s ~?= [3,5,7]

  , "toList should return empty list for empty set" ~:
      toList (empty 64) ~?= []

  , "isEmpty should return False for non-empty set" ~:
      isEmpty (add 0 (empty 64)) ~?= False

  , "capacity returns the correct value" ~:
      capacity (empty 64) ~?= 64

  , "adding out-of-range element should throw error" ~:
      TestCase (do
        let set = empty 64
        result <- (add 100 set `seq` return False) `catch` \(_ :: SomeException) -> return True
        assertBool "Exception was not thrown" result)

  , "contains returns False for out-of-range element" ~:
      contains 100 (empty 64) ~?= False
  ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

