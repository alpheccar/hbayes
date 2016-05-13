{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- | Conditional probability table

Conditional Probability Tables and Probability tables

-}
module Bayes.Factor.CPT(
   -- * CPT Factor
     CPT
   , changeVariableOrder
   , cptDivide
   , cptSum
   -- * Tests
   , testProductProject_prop
   , testAssocProduct_prop
   , testScale_prop
   , testProjectCommut_prop
   , testScalarProduct_prop
   , testProjectionToScalar_prop
   , debugCPT
   ) where


import Bayes.Factor 
import Bayes.Tools
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import System.Random(Random)
import Data.Maybe(fromJust,mapMaybe,isJust)
import Bayes.Factor.PrivateCPT
import Bayes.PrivateTypes
import Bayes.VariableElimination.Buckets(IsBucketItem(..))
import qualified Data.Vector.Unboxed as V

-- | Soft evidence factor can be used to initialize a factor
--instance Distribution CPT where 
--  createFactor dvs f = factorWithVariables dvs (factorToList f)

--- | Change the layout of values in the
--- factor to correspond to a new variable order
--- Used to import external files
changeVariableOrder :: DVSet s -- ^ Old order
                    -> DVSet s' -- ^ New order 
                    -> [Double] -- ^ Old values
                    -> [Double] -- ^ New values
changeVariableOrder (DVSet oldOrder) newOrder oldValues =
    let oldFactor = fromJust $ factorWithVariables oldOrder oldValues :: CPT
    in
    [factorValue oldFactor i | i <- forAllInstantiations newOrder]

instance Arbitrary CPT where
    arbitrary = do 
        theTVars <- arbitrary :: Gen (DVSet s)
        let theVars = fromDVSet theTVars
        --nbVertex <- choose (1,4) :: Gen Int
        --vertexNumbers <- generateWithoutReplacement nbVertex (0,50)
        --let dimensions = map (\i -> (DV (Vertex i)  (quickCheckVertexSize i))) vertexNumbers
        let valuelen = product (map dimension theVars)
        rndValues <- vectorOf valuelen (choose (0.0,1.0) :: Gen Double)
        return . fromJust . factorWithVariables theVars $ rndValues

-- | Test product followed by a projection when the factors have no
-- common variables


testScale_prop :: Double -> CPT -> Bool
testScale_prop s f = (factorNorm (s `factorScale` f)) `nearlyEqual` (s * (factorNorm f))

testProductProject_prop :: CPT -> CPT -> Property
testProductProject_prop fa fb = isEmpty ((factorVariables fa) `intersection` (factorVariables fb))  ==> 
    let r = factorProjectOut (factorVariables fb) (factorProduct [fa,fb])
        fa' = r `factorDivide` (factorNorm fb)
    in
    fa' `isomorphicFactor` fa

testScalarProduct_prop :: Double -> CPT -> Bool 
testScalarProduct_prop v f = (factorProduct [factorFromScalar v,f]) `isomorphicFactor` (v `factorScale` f)

testAssocProduct_prop :: CPT -> CPT -> CPT -> Bool
testAssocProduct_prop a b c = (factorProduct [factorProduct [a,b],c] `isomorphicFactor` factorProduct [a,factorProduct [b,c]]) &&
  (factorProduct [a,b,c] `isomorphicFactor` (factorProduct [factorProduct [a,b],c]) )

testProjectionToScalar_prop :: CPT -> Bool 
testProjectionToScalar_prop f = 
    let allVars = factorVariables f 
    in
    (factorProjectOut allVars f) `isomorphicFactor` (factorFromScalar (factorNorm f))

testProjectCommut_prop:: CPT -> Property 
testProjectCommut_prop f = length (factorVariables f) >= 3 ==>
    let a = take 1 (factorVariables f)
        b = take 1 . drop 1 $ factorVariables f 
        commuta = factorProjectOut a (factorProjectOut b f)
        commutb = factorProjectOut b (factorProjectOut a f)
    in
    commuta `isomorphicFactor` commutb



instance Show CPT where
    show (Scalar v) = "\nScalar Factor:\n" ++ show v
    show c@(Table [] _ v) = "\nEmpty CPT:\n"

    show c = displayFactorBody c    


instance FactorElement Double where 
    doubleValue = id 
    mkValue = id
    scale = (*)
    multiply = (*)
    divide _ 0 = 0
    divide a b = a / b 
    elementSum = (+)


instance Factor CPT where
    emptyFactor = _emptyFactor
    factorVariables = _factorVariables
    isScalarFactor = _isScalarFactor
    containsVariable = _containsVariable
    factorDimension = _factorDimension
    variablePosition = _variablePosition
    isUsingSameVariablesAs = _isUsingSameVariablesAs

    factorFromScalar = _factorFromScalar
    factorWithVariables = _factorWithVariables
    factorToList = _factorToList
    factorNorm = _factorNorm
    factorScale = _factorScale 
    factorValue = _factorValue
    factorStringValue f d = show (_factorValue f d)
    
    evidenceFrom = _evidenceFrom

    factorProduct = _factorProduct (Op (factorFromScalar 1.0) 1.0 multiply)
    
    factorProjectOut _ f@(Scalar v) = f
    factorProjectOut s f= cptFactorProjectOutWith (sum . map fst) s f

--  Divie two CPT
cptDivide :: CPT -> CPT -> CPT
cptDivide a b   | isScalarFactor a && isScalarFactor b = 
                    let va = factorValue a [] 
                        vb = factorValue b [] 
                    in 
                    factorFromScalar (divide va vb)
                | isScalarFactor a = 
                    let va = factorValue a [] 
                    in 
                    factorScale va b 
                | isScalarFactor b = 
                    let vb = factorValue b [] 
                    in 
                    if vb == 0.0 then factorFromScalar 0.0 else factorScale (1.0 / vb) a 
                | otherwise =
                    _factorProduct (Op (factorFromScalar 1.0) 1.0 divide) [b,a]


cptSum :: [CPT] -> CPT
cptSum = _factorProduct (Op (factorFromScalar 0.0) 0.0 elementSum)

instance IsBucketItem CPT where 
    scalarItem = isScalarFactor
    itemProduct = factorProduct
    itemProjectOut d = factorProjectOut [d]
    itemContainsVariable = containsVariable

instance MultiDimTable CPT where 
    elementStringValue = factorStringValue
    tableVariables = factorVariables
      
