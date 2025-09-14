-- Student's name: Paco Molina Cuenca
-- Student's group: 
-- Identity number (DNI if Spanish/passport if Erasmus): 
 
module SetChain ( 
    SetChain 
    , empty 
    , isEmpty 
    , pendingTransactions 
    , add 
    , getEpoch 
    , size 
    , validate 
    --  
    , addAll     
    , fold 
    , toList 
    ) where 
 
import qualified DataStructures.Set.LinearSet as S 
import qualified DataStructures.Dictionary.AVLDictionary as D 

--                    Mempool            History                 Epoch 
data SetChain a = SC (S.Set a) (D.Dictionary Integer (S.Set a)) Integer 
 
-- ------------------------------- 
-- DO NOT MODIFY THE CODE ABOVE 
-- ------------------------------- 

-- * Exercise a) 
empty :: SetChain a
empty = SC S.empty D.empty 0

-- * Exercise b) 
isEmpty :: SetChain a -> Bool
isEmpty (SC mempool history epoch) = S.isEmpty mempool && D.isEmpty history && epoch == 0

-- * Exercise c) 
getEpoch :: (Eq a) => a -> SetChain a -> Integer 
getEpoch trans (SC mempool history epoch) = getEpochRec (D.keys history)
  where 
    getEpochRec []     = -1
    getEpochRec (k:ks) = 
      case D.valueOf k history of 
        Nothing       -> -1 
        Just transSet -> if S.isElem trans transSet then k else getEpochRec ks
 
-- * Exercise d) 
size :: SetChain a -> Int 
size (SC _ history _) = sum [S.size transSet | transSet <- D.values history]
 
-- * Exercise e) 
pendingTransactions :: SetChain a -> Bool 
pendingTransactions (SC mempool _ _)
  | not(S.isEmpty mempool) = True 
  | otherwise              = False
 
-- * Exercise f) 
add :: (Eq a) => a -> SetChain a -> SetChain a 
add trans (SC mempool history epoch) = SC (S.insert trans mempool) history epoch
 
-- * Exercise g) 
validate :: (Ord a) => SetChain a -> SetChain a 
validate sc@(SC mempool history epoch)
  | not(pendingTransactions sc) = sc 
  | alreadyValidatedTrans       = error "transaction already validated"
  | otherwise                   = SC S.empty (D.insert epoch mempool history) (epoch + 1)
  where 
    alreadyValidatedTrans = or [S.isElem trans transSet | let transSet = foldr S.union S.empty (D.values history), trans <- S.fold (:) [] mempool]
 
-- * Exercise h) 
addAll :: (Eq a) => [a] -> SetChain a -> SetChain a 
addAll xs sc = foldr add sc xs
 
-- * Exercise i) 
fold :: (Ord a) => (a -> b -> b) -> b -> SetChain a -> b 
fold f z (SC _ history _) = let transSet = foldr S.union S.empty (D.values history) in S.fold f z transSet
        
-- * Exercise j) 
toList :: (Ord a) => SetChain a -> [a] 
toList = fold (:) []

-- ------------------------------- 
-- DO NOT MODIFY THE CODE BELOW 
-- ------------------------------- 
 
instance (Show a) => Show (SetChain a) where 
  show (SC s h e) = concat ["SetChain(", show s, ", ", show h, ", ", show e, ")"] 
 
instance (Eq a) => Eq (SetChain a) where 
  (SC m1 h1 e1) == (SC m2 h2 e2) = m1 == m2 && h1 == h2 && e1 == e2
