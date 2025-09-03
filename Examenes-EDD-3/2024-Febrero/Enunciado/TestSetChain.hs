import Test.HUnit
import qualified DataStructures.Set.LinearSet as S
import qualified DataStructures.Dictionary.AVLDictionary as D

-- Asumimos que el tipo SetChain y las funciones están importadas y definidas

-- Un ejemplo sencillo de transacciones
tx1, tx2, tx3 :: Int
tx1 = 1
tx2 = 2
tx3 = 3

-- Un SetChain vacío para comenzar
emptySC :: SetChain Int
emptySC = empty

-- Test 1: empty y isEmpty
testEmptyIsEmpty = TestCase $ do
  assertBool "empty is empty" (isEmpty emptySC)
  let sc1 = add tx1 emptySC
  assertBool "after add, not empty" (not $ isEmpty sc1)

-- Test 2: add y pendingTransactions
testAddPending = TestCase $ do
  let sc1 = add tx1 emptySC
  assertBool "pending transactions after add" (pendingTransactions sc1)
  let sc2 = validate sc1
  assertBool "no pending transactions after validate" (not $ pendingTransactions sc2)

-- Test 3: getEpoch
testGetEpoch = TestCase $ do
  let sc1 = add tx1 emptySC
  assertEqual "getEpoch tx1 before validate" (-1) (getEpoch tx1 sc1)
  let sc2 = validate sc1
  assertEqual "getEpoch tx1 after validate" 0 (getEpoch tx1 sc2)

-- Test 4: size
testSize = TestCase $ do
  let sc1 = add tx1 $ add tx2 emptySC
  let sc2 = validate sc1
  assertEqual "size after adds, before validate" 0 (size sc1)
  assertEqual "size after validate" 1 (size sc2)
  let sc3 = add tx3 sc2
  assertEqual "size after add post validate" 1 (size sc3)

-- Test 5: fold y toList coherencia
testFoldToList = TestCase $ do
  let sc1 = addAll [tx1, tx2] emptySC
  let sc2 = validate sc1
  let lst = toList sc2
  assertBool "toList contains tx1" (tx1 `elem` lst)
  assertBool "toList contains tx2" (tx2 `elem` lst)
  assertEqual "fold counts elements" (length lst) (fold (\_ acc -> acc + 1) 0 sc2)

-- Test 6: validate error on duplicate
testValidateDuplicate = TestCase $ do
  let sc1 = addAll [tx1] emptySC
  let sc2 = validate sc1
  let sc3 = add tx1 sc2 -- trying to add duplicate tx1 again
  let result = (validate sc3 >> return True) `catch` (\(ErrorCall _) -> return False)
  res <- result
  assertBool "validate duplicate throws error" (not res)

tests :: Test
tests = TestList
  [ TestLabel "Empty and isEmpty" testEmptyIsEmpty
  , TestLabel "Add and pendingTransactions" testAddPending
  , TestLabel "getEpoch" testGetEpoch
  , TestLabel "Size function" testSize
  , TestLabel "Fold and toList coherence" testFoldToList
  , TestLabel "Validate throws on duplicate" testValidateDuplicate
  ]

main :: IO Counts
main = runTestTT tests

