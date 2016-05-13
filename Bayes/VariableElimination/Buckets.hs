{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{- | Bucket algorithms for variable elimination with enough flexibility to
also work with influence diagrams.
    
-}
module Bayes.VariableElimination.Buckets(
    -- * Types 
      Buckets(..)
    , EliminationOrder(..)
    , IsBucketItem(..)
    -- * Functions 
    , createBuckets 
    , getBucket 
    , updateBucket 
    , addBucket 
    , removeFromBucket 
    , marginalizeOneVariable
    ) where 

import Bayes.PrivateTypes
import qualified Data.Map as M
import Data.List(partition,minimumBy,(\\),find,foldl')
import Data.Maybe(fromJust)

-- | Elimination order
type EliminationOrder dv = [dv]

-- | Used for bucket elimination. Factor are organized by their first DV
data Buckets f = Buckets !(EliminationOrder DV) !(M.Map DV [f])

instance Show f => Show (Buckets f) where 
  show (Buckets v m) = "BUCKET\n" ++ show v ++ "\n" ++ concatMap disp (M.toList m)
   where
    disp (v,f) = "Bucket for " ++ show v ++ "\n" ++ concatMap dispElem f ++ "\n----\n"
    dispElem f = show f ++ "\n"

-- | Operations needed to process a bucket items
class IsBucketItem f where 
    scalarItem :: f -> Bool 
    itemProduct :: [f] -> f
    itemProjectOut :: DV -> f -> f
    itemContainsVariable :: f -> DV  -> Bool



addDVToBucket :: IsBucketItem f => ([f],M.Map DV [f]) -> DV -> ([f],M.Map DV [f]) 
addDVToBucket (rf, m) dv  =
  let (fk,remaining) = partition (flip itemContainsVariable dv) rf
  in 
  (remaining, M.insert dv fk m)

createBuckets ::  (IsBucketItem f) 
              => [f] -- ^ Factor to use for computing the marginal one
              -> EliminationOrder DV -- ^ Variables to eliminate
              -> EliminationOrder DV -- ^ Remaining variables
              -> Buckets f 
createBuckets s e r = 
  let -- We put the selected variables for elimination in the right order at the beginning
      -- Which means the function can work with a partial order which is completed with other
      -- variables by default.
      theOrder = e ++ r
      (_,b) = foldl' addDVToBucket (s,M.empty) theOrder
  in
  Buckets theOrder b

-- | Get the factors for a bucket
getBucket :: DV 
          -> Buckets f 
          -> [f]
getBucket dv (Buckets _ m) = fromJust $ M.lookup dv m

-- | Update bucket
updateBucket :: IsBucketItem f
             => DV -- ^ Variable that was eliminated
             -> f -- ^ New factor resulting from this elimination
             -> Buckets f 
             -> Buckets f 
updateBucket dv f b@(Buckets e m) = 
  if scalarItem f 
    then 
      Buckets (remainingVarsToProcess e) (M.insert dv [f] m)
    else
      let b' = removeFromBucket dv b
      in
      addBucket b' f 
 where 
  remainingVarsToProcess [] = []
  remainingVarsToProcess l = tail l

-- | Add a factor to the right bucket
addBucket :: IsBucketItem f => Buckets f -> f -> Buckets f
addBucket (Buckets e b) f = 
  let inBucket = find (f `itemContainsVariable`) e
  in 
  case inBucket of 
    Nothing -> Buckets e b
    Just bucket -> Buckets e (M.insertWith' (++) bucket [f] b)

-- | Remove a variable from the bucket
removeFromBucket :: DV -> Buckets f -> Buckets f 
removeFromBucket dv (Buckets [] m) = Buckets [] (M.delete dv m) 
removeFromBucket dv (Buckets e m) = Buckets (tail e) (M.delete dv m) 

marginalizeOneVariable :: IsBucketItem f => Buckets f -> DV -> Buckets f
marginalizeOneVariable currentBucket dv   = 
  let fk = getBucket dv currentBucket
      p = itemProduct fk
      f' = itemProjectOut dv p
  in
  updateBucket dv f' currentBucket
