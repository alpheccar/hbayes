{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | Private functions used to implement CPT

-}
module Bayes.Factor.PrivateCPT(
   -- * Functions
     cptFactorProjectOutWith
   -- ** Private functions
   , FactorElement(..)
   , DecisionFactor(..)
   , Operator(..)
   , _emptyFactor
   , _factorVariables
   , _isScalarFactor
   , _containsVariable
   , _factorDimension
   , _variablePosition
   , _factorFromScalar
   , _factorWithVariables
   , _factorToList
   , _factorNorm
   , _factorScale 
   , _factorValue
   , _evidenceFrom
   , _factorProduct
   , _isUsingSameVariablesAs
   , CPT
   , MAXCPT
   , PrivateCPT(..)
   , PossibleInstantiations
   , convertToMaxFactor
   , convertToNormalFactor 
   , decisionFactor
   , debugCPT
   , privateFactorValue
  ) where

import qualified Data.Vector.Generic as GV

import qualified Data.Vector.Unboxed as V
import Bayes.PrivateTypes
import qualified Data.IntMap as IM
import Bayes.Factor
import Data.Vector.Generic((!))
import Data.Maybe(fromJust,mapMaybe,isJust)
import qualified Data.List as L
import Data.List(partition)
import qualified Data.Vector as NV


{-
 
CPT used for normal CPT and MAXCPT

-}
-- | Mainly used for conditional probability table like p(A B | C D E) but the normalization to 1
-- is not imposed. And the conditionned variables are not different from the conditionning ones.
-- The dimensions for each variables are listed.
-- The variables on the left or right of the condition bar are not tracked. What's matter is that
-- it is encoding a function of several variables.
-- Marginalization of variables will be computed from the bayesian graph where
-- the knowledge of the dependencies is.
-- So, this same structure is used for a probability too (conditioned on nothing)
data PrivateCPT v a = Table {
                      dimensions :: ![DV] -- ^ Dimensions for all variables
                    , mapping :: !(IM.IntMap Int) -- ^ Mapping from vertex number to position in dimensions
                    , values :: !(v a) -- ^ Table of values
                    }
                    | Scalar !a  deriving(Eq)

debugCPT (Scalar d) = do 
   putStrLn "SCALAR CPT"
   print d
   putStrLn ""

debugCPT (Table d m v) = do 
    putStrLn "CPT"
    print d 
    putStrLn ""
    print m 
    putStrLn ""
    print v
    putStrLn ""

-- | Convert a factor to MAXCPT to change the behavior of inference algorithms and use max
convertToMaxFactor :: CPT -> MAXCPT
convertToMaxFactor (Scalar v) = Scalar (v,[])
convertToMaxFactor (Table d m v) =
  let newValues = NV.fromList . map (\x -> (x,[])) . V.toList $ v
  in
  Table d m newValues

type DecisionFactor = PrivateCPT (NV.Vector) DVI 




-- | Return an array of instantiations. Used to compute decisions
-- in an influence diagram
decisionFactor :: MAXCPT -> DecisionFactor
decisionFactor (Scalar (_,l)) = Scalar (head . head $ l) 
decisionFactor (Table d m v) =
  let extractElem = head . head -- We only have ONE instantiated variable in influence diagram. So no need
      -- for a list of list (list of several variables)
      newValues = NV.fromList . map extractElem . map snd . NV.toList $ v
  in
  Table d m newValues



convertToNormalFactor :: MAXCPT -> CPT 
convertToNormalFactor (Scalar a) = Scalar (fst a)
convertToNormalFactor (Table d m v) =
  let newValues = V.fromList . map fst . NV.toList $ v
  in
  Table d m newValues

type CPT = PrivateCPT (V.Vector) Double

type PossibleInstantiations = [DVISet]

type MAXCPT = PrivateCPT (NV.Vector) (Double,PossibleInstantiations)

class FactorElement a where 
    doubleValue :: a -> Double 
    mkValue :: Double -> a
    scale :: Double -> a -> a
    multiply :: a -> a -> a
    divide :: a -> a -> a
    elementSum ::  a -> a -> a

_isUsingSameVariablesAs :: PrivateCPT v a -> PrivateCPT v a -> Bool
_isUsingSameVariablesAs (Table va _ _) (Table vb _ _) = va == vb

_emptyFactor :: GV.Vector v a => PrivateCPT v a
_emptyFactor = Table [] IM.empty GV.empty

_factorVariables :: PrivateCPT v a -> [DV]
_factorVariables (Table v _ _) = v
_factorVariables (Scalar _) = []

_factorToList :: (FactorElement a, GV.Vector v a) => PrivateCPT v a -> [Double]
_factorToList (Scalar v) = [doubleValue v]
_factorToList (Table _ _ v) = map doubleValue $ GV.toList v

_isScalarFactor :: PrivateCPT v a -> Bool 
_isScalarFactor (Scalar _) = True
_isScalarFactor _ = False

_factorFromScalar :: FactorElement a => Double -> PrivateCPT v a
_factorFromScalar v = Scalar (mkValue v) 

_factorDimension :: Factor (PrivateCPT v a) => PrivateCPT v a -> Int
_factorDimension f@(Table _ _ _) = product . map dimension . factorVariables$ f
_factorDimension _ = 1

_containsVariable :: PrivateCPT v a -> DV  -> Bool
_containsVariable (Table _ m _) (DV (Vertex i) _)   = IM.member i m
_containsVariable (Scalar _) _ = False

_factorWithVariables :: (FactorElement a, GV.Vector v a) => [DV] -> [Double] -> Maybe (PrivateCPT v a)
_factorWithVariables dv v = createCPTWithDims dv (GV.fromList (map mkValue v))

_factorNorm :: (FactorElement a, GV.Vector v a) => PrivateCPT v a -> Double 
_factorNorm f@(Table d _ vals) = 
    let vars = DVSet d
        strides = indexStrides vars
    in
    sum [ doubleValue(vals!(indexPosition strides x)) | x <- indicesForDomain vars]
_factorNorm (Scalar v) = (doubleValue v)


_variablePosition :: PrivateCPT v a -> DV -> Maybe Int
_variablePosition (Table _ m _) (DV (Vertex i) _) = let r = IM.lookup i m in r `seq` r
_variablePosition (Scalar _) _ = Nothing

_factorScale :: (FactorElement a, GV.Vector v a, Factor (PrivateCPT v a)) => Double -> PrivateCPT v a -> PrivateCPT v a
_factorScale s (Scalar v) = Scalar (scale s v)
_factorScale s f@(Table d _ vals) = 
    let vars = DVSet d
        strides = indexStrides vars
        newValues = GV.force . GV.fromList $ map (scale s) [ vals!(indexPosition strides x) | x <- indicesForDomain vars]
    in 
    fromJust $ createCPTWithDims (factorVariables f) newValues

privateFactorValue :: (GV.Vector v a) => PrivateCPT v a  -> [DVI] -> a
{-# INLINE privateFactorValue #-}
privateFactorValue (Scalar v) _ = v 
privateFactorValue f@(Table d _ v) i = 
    let vars = DVSet d
        (dv,pos) = instantiationDetails i
        strides = indexStridesFor vars dv
    in 
    v!(indexPosition strides pos)

_evidenceFrom :: (FactorElement a, GV.Vector v a) => [DVI] -> Maybe (PrivateCPT v a)
_evidenceFrom [] = Nothing 
_evidenceFrom l = 
    let (variables,index) = instantiationDetails l
        DVSet nakedVars = variables
        setValueForIndex i = if i == index then mkValue 1.0 else mkValue 0.0 
        values = GV.force . GV.fromList $ map setValueForIndex $ indicesForDomain variables
    in
    createCPTWithDims nakedVars values

_factorValue :: (FactorElement a, GV.Vector v a) => PrivateCPT v a  -> [DVI] -> Double
{-# INLINE _factorValue #-}
_factorValue f d = doubleValue (privateFactorValue f d)

data Operator f a = Op f a (a -> a -> a)

_factorProduct :: (FactorElement a, GV.Vector v a, Factor (PrivateCPT v a)) 
               => Operator (PrivateCPT v a) a
               -> [PrivateCPT v a] 
               -> PrivateCPT v a
_factorProduct (Op fUnit opUnit op) [] = fUnit
_factorProduct (Op fUnit opUnit op) l = 
    let nakedVars = L.foldr1 union . map factorVariables $ l
        allVars = DVSet nakedVars
        (scalars,cpts) = partition isScalarFactor l
        stridesFromCPT (Table d _ _) = indexStridesFor (DVSet d) allVars
        elementProduct [] = opUnit
        elementProduct l = foldr1 op l
        ps = elementProduct . map (flip privateFactorValue undefined) $ scalars
        cptsStrides = map stridesFromCPT cpts
    in 
    if L.null cpts 
        then 
            Scalar ps
        else
            let getFactorValueAtIndex i (strides,factor@(Table _ _ vals)) = vals!(indexPosition strides i)
                instantiationProduct instantiation = elementProduct . map (getFactorValueAtIndex instantiation) $ (zip cptsStrides cpts)
                values = GV.force $ GV.fromList [op ps (instantiationProduct x) | x <- indicesForDomain allVars]
            in 
            fromJust $ createCPTWithDims nakedVars values


-- | Genereic projection function which can use any function to project out
-- some variables in a CPT. Used to implement the MAP queries.
cptFactorProjectOutWith :: (FactorElement a, GV.Vector v a)
                        => ([(a,DVISet)] -> a) -- ^ Summing or maximizing function
                        -> [DV] -- ^ Variables to sum or maximize
                        -> PrivateCPT v a 
                        -> PrivateCPT v a
cptFactorProjectOutWith sumF s f@(Table d _ v) = 
            let variablesToSum = s
                variablesToKeep = d `difference` s 
                keepSet = DVSet variablesToKeep
                sumSet = DVSet variablesToSum 
                
                values = 
                    let strides = indexStridesFor (DVSet d) (DVSet $ variablesToKeep ++ variablesToSum)
                    in
                    GV.force . GV.fromList $ do 
                      keepIndex <- indicesForDomain keepSet 
                      let l = do
                            sumIndex <- indicesForDomain sumSet 
                            let result = v!(indexPosition strides $ combineIndex strides keepIndex sumIndex) 
                                sumInstantiation = instantiation sumSet sumIndex
                            return (result,sumInstantiation)
                      return $ (sumF l)
            in
            fromJust $ createCPTWithDims variablesToKeep values

-- | Create a CPT given some dimensions and a list of Doubles.
-- Returns nothing is the length are not coherents.
createCPTWithDims :: GV.Vector v a => [DV] -> v a -> Maybe (PrivateCPT v a)
createCPTWithDims [] v = Just (Scalar (GV.head v))
createCPTWithDims dims values = 
    let createDVIndex i (DV (Vertex v) _)  = (v,i)
        m = IM.fromList . zipWith createDVIndex ([0,1..]::[Int]) $ dims
        p = product (map dimension dims) 
    in
    if GV.length values == p
        then
            Just $! m `seq` Table dims m (GV.force values)
        else 
            Nothing



{-

Stride management

-}


-- | Used to combined the keep and sum indices in the factor projection
combineIndex :: Strides s'' -> MultiIndex s -> MultiIndex s' -> MultiIndex s''
{-# INLINE combineIndex #-}
combineIndex _ (MultiIndex la) (MultiIndex lb) = MultiIndex (la V.++ lb)



newtype Strides s = Strides (V.Vector Int) deriving(Eq,Show)

-- | Generate strides to read the first CPT using an index having meaning in the second CPT
indexStridesFor :: DVSet s -- ^ DVSet to be read
                -> DVSet s' -- ^ DVSet to interpret the index
                -> Strides s'
indexStridesFor dr@(DVSet drvars) di@(DVSet divars) =
    let Strides originStrides = indexStrides dr
        reference = zip drvars (V.toList originStrides) 
        getNewStrides dv = maybe 0 id (lookup dv reference)
    in 
    Strides $ (V.force . V.fromList $ map getNewStrides divars)
    

-- | Generate the strides to read a given factor using a multiindex
-- using the same order as the factor variables
indexStrides :: DVSet s -> Strides s
indexStrides d@(DVSet dvars)  = 
    let dim = map dimension dvars
        pos' = scanr (*) (1::Int) (tail dim)
    in 
    Strides (V.force . V.fromList $ pos')

-- | Convertion of a multiindex to its
-- position inside of the data vector of a 'CPT'
indexPosition :: Strides s -> MultiIndex s -> Int
{-# INLINE indexPosition #-}
indexPositions _ []  = 0
indexPosition (Strides pos') (MultiIndex pos) = V.sum . V.zipWith (\x y -> x * y) pos' $ pos


