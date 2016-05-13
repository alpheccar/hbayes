{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{- | Factors

-}
module Bayes.Factor(
 -- * Factor
   Factor(..)
 , Distribution(..)
 , MultiDimTable(..)
 , isomorphicFactor
 , normedFactor
 , displayFactorBody
 , changeFactorInFunctor 
 , FactorContainer(..)
 -- * Set of variables 
 , Set(..)
 , BayesianDiscreteVariable(..)
 , BayesianVariable(..)
 -- * Implementation
 , Vertex(..)
 -- ** Discrete variables and instantiations
 , DV(..)
 , TDV
 , DVSet(..)
 , DVI
 , DVISet
 , InstantiationValue(..)
 , tdvi
 , tdv
 , setDVValue
 , instantiationVariable
 , variableVertex
 , (=:)
 , forAllInstantiations
 , factorFromInstantiation
 ) where

import Data.Maybe(fromJust)
import Control.Monad
import Bayes.PrivateTypes
import Bayes.Tools
import qualified Data.Vector.Unboxed as V
import Text.PrettyPrint.Boxes hiding((//))
import Bayes.VariableElimination.Buckets(IsBucketItem(..))

--import Debug.Trace

--debug a = trace ("\nDEBUG\n" ++ show a ++ "\n") a



-- | A distribution which can be used to create a factor
class Distribution d where
  -- | Create a factor from variables and a distributions for those variables
  createFactor :: Factor f => [DV] -> d -> Maybe f

instance Real a => Distribution [a] where 
  createFactor dvs l = factorWithVariables dvs (map realToFrac l)


-- | Change factor in a functor (only factor values should have been changed)
-- It assumes that the variables of a factor are enough to identify it.
-- If the functor is containing several factors with same set of variables then it
-- won't give a meaningful result.
-- So it should be used only on functor derived from a Bayesian Network.
changeFactorInFunctor :: (Factor f, Functor m) => f -> m f -> m f
changeFactorInFunctor f g = 
  let replaceFactor cf | cf `isUsingSameVariablesAs` f = f 
                       | otherwise = cf
  in
  fmap replaceFactor g 

-- | Structure containing factors which can be replaced.
-- It is making sense when the factors are related to the nodes of a Bayesian
-- network. 
class FactorContainer m where 
   changeFactor :: (IsBucketItem f,Factor f) => f -> m f -> m f 

instance FactorContainer [] where 
    changeFactor = changeFactorInFunctor

-- | A vertex associated to another value (variable dimension, variable value ...)
class LabeledVertex l where
    variableVertex :: l -> Vertex


-- | Convert a variable instantation to a factor
-- Useful to create evidence factors
factorFromInstantiation :: Factor f => DVI -> f
factorFromInstantiation (DVI dv a) = 
    let setValue i = if i == a then 1.0 else 0.0 
    in
    fromJust . factorWithVariables [dv] . map (setValue) $ [0..dimension dv-1]




instance LabeledVertex DVI where
    variableVertex (DVI v _) = variableVertex v

instance LabeledVertex DV where
    variableVertex (DV v _) = v


-- | Norm the factor
normedFactor :: Factor f => f -> f 
normedFactor f = factorDivide f (factorNorm f)

-- | A factor as used in graphical model
-- It may or not be a probability distribution. So it has no reason to be
-- normalized to 1
class Factor f where
    -- | When all variables of a factor have been summed out, we have a scalar
    isScalarFactor :: f -> Bool 
    -- | An empty factor with no variable and no values
    emptyFactor :: f
    -- | Check if a given discrete variable is contained in a factor
    containsVariable :: f -> DV  -> Bool
    -- | Give the set of discrete variables used by the factor
    factorVariables :: f -> [DV]    
    -- | Return A in P(A | C D ...). It is making sense only if the factor is a conditional propbability
    -- table. It must always be in the vertex corresponding to A in the bayesian graph
    factorMainVariable :: f -> DV
    factorMainVariable f = let vars = factorVariables f 
      in
      case vars of 
        [] -> error "Can't get the main variable of a scalar factor"
        (h:_) -> h 
    -- | Create a new factors with given set of variables and a list of value
    -- for initialization. The creation may fail if the number of values is not
    -- coherent with the variables and their levels.
    -- For boolean variables ABC, the value must be given in order
    -- FFF, FFT, FTF, FTT ...
    factorWithVariables :: [DV] -> [Double] -> Maybe f
    -- | Value of factor for a given set of variable instantitation.
    -- The variable instantion is like a multi-dimensional index.
    factorValue :: f -> [DVI] -> Double

    -- | String representation of a factor value 
    factorStringValue :: f -> [DVI] -> String

    -- | Position of a discrete variable in te factor (p(AB) is differennt from p(BA) since values
    -- are not organized in same order in memory)
    variablePosition :: f -> DV -> Maybe Int
    -- | Dimension of the factor (number of floating point values)
    factorDimension :: f -> Int
    
    -- | Norm of the factor = sum of its values
    factorNorm :: f -> Double 
    

    -- | Scale the factor values by a given scaling factor
    factorScale :: Double -> f -> f

    -- | Create a scalar factor with no variables
    factorFromScalar :: Double -> f

    -- | Create an evidence factor from an instantiation.
    -- If the instantiation is empty then we get nothing
    evidenceFrom :: [DVI] -> Maybe f

    -- | Test if two factors are coding for the same probability dependence.
    -- It does not test if the factors are equal (same probabilities) but just
    -- if they involve the same variables so are linked to the same
    -- node in the Bayesian network
    isUsingSameVariablesAs ::  f -> f -> Bool
    

    -- | Divide all the factor values
    factorDivide :: f -> Double -> f
    factorDivide f d = (1.0 / d) `factorScale` f 

    factorToList :: f -> [Double]

    -- | Multiply factors. 
    factorProduct :: [f] -> f

    -- | Project out a factor. The variable in the DVSet are summed out
    factorProjectOut :: [DV] -> f -> f

    -- | Project to. The variable are kept and other variables are removed
    factorProjectTo :: [DV] -> f -> f 
    factorProjectTo s f = 
        let alls = factorVariables f 
            s' = alls `difference` s 
        in 
        factorProjectOut s' f

-- | Test equality of two factors taking into account the fact
-- that the variables may be in a different order.
-- In case there is a distinction between conditionned variable and
-- conditionning variables (imposed from the exterior) then this
-- comparison may not make sense. It is a comparison of
-- function of several variables which no special interpretation of the
-- meaning of the variables according to their position.
isomorphicFactor :: Factor f => f -> f -> Bool
isomorphicFactor fa fb = maybe False (const True) $ do 
    let sa = factorVariables fa 
        sb = factorVariables fb 
        va = DVSet sa 
        vb = DVSet sb
    guard (sa `equal` sb)
    guard (factorDimension fa == factorDimension fb)
    guard $ and [factorValue fa ia `nearlyEqual` factorValue fb ia | ia <- forAllInstantiations va]
    return ()

{-

Following functions are used to typeset the factor when displaying it

-}

-- | Class used to display multidimensional tables
class MultiDimTable f where 
    elementStringValue :: f -> [DVI] -> String
    tableVariables :: f -> [DV]   

-- | Display a variable name and its size
vname :: Int -> DVI -> Box
vname vc i = text $ "v" ++ show vc ++ "=" ++ show (instantiationValue i)

dispFactor :: MultiDimTable f => f -> DV -> [DVI] -> [DV] -> Box
dispFactor cpt h c [] = 
    let dstIndexes = allInstantiationsForOneVariable h
        dependentIndexes =  reverse c
        factorValueAtPosition p = 
            let v = elementStringValue cpt p
            in
            text v
    in
    vsep 0 center1 . map (factorValueAtPosition . (:dependentIndexes)) $ dstIndexes

dispFactor cpt dst c (h@(DV (Vertex vc) i):l) = 
    let allInst = allInstantiationsForOneVariable h
    in
    hsep 1 top . map (\i -> vcat center1 [vname vc i,dispFactor cpt dst (i:c) l])  $ allInst

displayFactorBody :: MultiDimTable f => f -> String 
displayFactorBody c = 
        let d = tableVariables c
            h@(DV (Vertex vc) _) = head d
            table = dispFactor c h [] (tail d)
            dstIndexes = map head (forAllInstantiations . DVSet $ [h])
            -- In P(A | B ...), the dst column is containing the possible values for the
            -- variables A with a header made of space to be aligned with the other part of the table.
            -- In the other part of the table, this header is containing the variable values for the other varibles
            dstColumn = vcat center1 $ replicate (length d - 1) (text "") ++ map (vname vc) dstIndexes
        in
        "\n" ++ show d ++ "\n" ++ render (hsep 1 top [dstColumn,table])
