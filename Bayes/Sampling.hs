{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{- | Sampling

Samplers for Bayesian network inferences

-}
module Bayes.Sampling(
  -- * Types 
     Sampler(..)
   , Sample(..)
  -- * Sampling
   , runSampling
   , topologicalOrder
  -- * Sampling schemes
   , discreteAncestralSampler
   , gibbsSampler
   , gibbsMCMCSampler
  -- * Sampling results
   , samplingHistograms
   , histogram
  -- * For continuous distributions 
   , ContinuousNetwork(..) 
   , ContinuousSample(..)
   , Distri(..)
   , continuousMCMCSampler
) where 


import Bayes
import Bayes.Factor
import Control.Monad.Reader 
import Data.Maybe(fromJust)
import Bayes.PrivateTypes
import System.Random.MWC.CondensedTable
import System.Random.MWC.Distributions(normal)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV(write,read)

import System.Random.MWC(GenIO,create,withSystemRandom,asGenIO,uniformR,Variate)

--import Debug.Trace 

--debug s a = trace ("DEBUG:\n" ++ s ++ show a ++ "\n") a

-- | A graph of sampling functions (parental sampler or markov blanket sampler)
-- It is used to generate new samples.
-- The vertex is implicit in the function definition
-- Which means that if a function is for updating vertex V then it will be the value of vertex
-- V in the sampler graph.
type SamplerGraph g a = BayesianNetwork g (Sample g a -> IO a)

-- | Sampling function for a graph.
-- Generate a new sample and update the state. 
type SamplingScheme g b a = GenIO -> SamplerGraph g a -> b -> Sample g a -> IO (b,Sample g a)

-- | Sampler defining the behavior of a sampling algorithms (init value,
-- sample generation, how to select nodes in the grapg)
data Sampler g a = forall b . Sampler !b !(GenIO -> IO (Sample g a)) !(GenIO -> SamplerGraph g a) !(SamplingScheme g b a)


data Distri = D !CV !(DistributionF DirectedSG (Double,Double) CVI)

type ContinuousNetwork = SBN Distri

type ContinuousSample = SBN CVI

-- | Get current value of function using current network values
distributionOnNetwork ::(DistributionF DirectedSG (Double,Double) CVI) -> Vertex -> Reader (Sample DirectedSG CVI) Double 
distributionOnNetwork (Distri _ f) v = do 
  r <- ask 
  let inst = fromJust . vertexValue r $ v 
  f inst

instance Graph g => Show (Sample g [DVI]) where 
    show = show . allNodes

instance Graph g => Show (Sample g CVI) where 
    show = show . allNodes

instance Graph g => Show (Sample g [(Double,Double,Double)]) where 
    show = show . allNodes

{-

Ancestral sampling 

-}

-- | Return the vertices in topological order
topologicalOrder :: DirectedGraph g => g a b -> [Vertex] 
topologicalOrder g = _topologicalOrder g [] 
 where
  _topologicalOrder g current = case rootNode g of 
    Nothing -> reverse current
    Just r -> _topologicalOrder (removeVertex r g) (r:current)

-- | Generate a new sample for vertex based upon the sampler functions
-- and the current sample
generateNewSample :: Graph g => SamplerGraph g a  -> Sample g a -> Vertex -> IO (Sample g a)
generateNewSample sampler sample theVertex = do
   let vertexSampler = fromJust $ vertexValue sampler theVertex 
   newValue <- vertexSampler sample 
   return . fromJust . changeVertexValue theVertex newValue $ sample 

-- | Update all vertices in the current sample and generate a new sample.
-- It is done using ancestral sampling
ancestralSampling :: DirectedGraph g => SamplingScheme g [Vertex] a
ancestralSampling gen sampler nodes s  = do 
  r <- foldM (generateNewSample sampler) s nodes
  return (nodes,r)

-- | Select a vertex and generate a new sample using gibbs sampling
gibbsSampling :: DirectedGraph g => SamplingScheme g (CondensedTable V.Vector Vertex) a
gibbsSampling gen samplerGraph table s = do 
  aVertex <- genFromTable table gen 
  r <- generateNewSample samplerGraph s aVertex
  return (table,r)

-- | Sequence a list of the SAME graphs but with different node values
sequenceG :: (FunctorWithVertex g, Graph g) => [g a b] -> g a [b]
sequenceG [] = error "Can't sequence an empty list of graphs"
sequenceG l@(h:_) = 
  let setVertexList vertex oldValue = map (\g -> fromJust . vertexValue g $ vertex) l
  in 
  fmapWithVertex setVertexList h 

_range :: Double -> Double -> [Double] -> (Double,Double)
_range mi ma [] = (mi,ma) 
_range mi ma (h:t) | h < mi = _range h ma t 
                   | h > ma = _range mi h t 
                   | otherwise = _range mi ma t 

range :: [Double] -> (Double,Double)
range = _range (1/0) (-1/0)

-- | Compute the histogram of values
histogram :: Int -- ^ Bins 
          -> [Double] -- ^ Samples 
          -> [(Double,Double,Double)] -- ^ Histogram  (minBound,maxBound, value)
histogram bins values | bins <= 0 = error "Bins must be >= 1" 
                      | bins == 1 =
  let (bmin,bmax) = range values 
  in 
  [(bmin,bmax,1.0)]
                      | otherwise = 
  let (bmin,bmax) = range values 
  in
  if bmax == bmin 
    then 
      [(bmin,bmax,1.0)]
    else 
      let delta = (bmax - bmin) / fromIntegral bins
          v = UV.replicate bins 0.0
          orMAX a | a >= bins = bins - 1 
                  | otherwise = a
          countSample value = UV.modify (\s -> do 
            --when (isNaN value) $ error "NaN"
            let index = orMAX $ floor ((value - bmin) / delta) 
            --when (index < 0) $ error ("index == " ++ show index ++ show " " ++ show value) 
            --when (index >= bins) $ error ("index == " ++ show index ++ show " " ++ show value)
            d <- MV.read s index 
            MV.write s index (d+1)) 
          v' = foldr countSample v values
          t = UV.sum v'
          scaled = UV.map (/ t) v'
          addBounds value i = (bmin + delta * i ,bmin + delta + delta*i,value)
      in 
      zipWith addBounds (UV.toList scaled) [0..]

-- | Generate a graph of sampling histogram for each variable
-- So, for a vertex v we have the posterior values p(v)
samplingHistograms :: (InstantiationValue i v,BayesianVariable i, FunctorWithVertex g, Graph g) 
                   => Int -- ^ Number of bins
                   -> [Sample g i] -- ^ Samples
                   -> Sample g [(Double,Double,Double)] -- ^ Histogram with start bins and bin content
samplingHistograms bins g' = 
  let g = sequenceG g' 
  in
  fmapWithVertex (createHistogram bins g) g
 where 
  createHistogram bins g vertex l =  histogram bins . map toDouble $ l

-- | Sample a bayesian network using a given sampling scheme
runSampling :: (DirectedGraph g, FunctorWithVertex g) 
            => Int -- ^ Number of used samples
            -> Int -- ^ Burnin samples before the used samples
            -> Sampler g a -- ^ Sampler
            -> IO [Sample g a] -- ^ List of generated samples
runSampling n b (Sampler initState startSample sampler samplingScheme) = do 
    withSystemRandom . asGenIO $ \gen -> do
       start <- startSample gen
       l <- _runAncestralSampling gen (n+b) initState start []
       return (drop b $ l)
  where 
    _runAncestralSampling gen n istate i r | n <= 1 = return $ reverse (i:r)
                                           | otherwise = do
                                               (newState,i') <- samplingScheme gen (sampler gen)  istate i
                                               _runAncestralSampling gen (n-1) newState i' (i':r)  


{-

Discrete variable sampling

-}  

-- | Gibbs sampling
gibbsSampler :: (Factor f, FunctorWithVertex g, DirectedGraph g) 
             => BayesianNetwork g f -- ^ Bayesian network
             -> [DVI] -- ^ Evidence
             -> Sampler g DVI -- ^ Sampler
gibbsSampler g evidence = 
  let isEvidence v = v `elem` (map vertex evidence)
      v = filter (not . isEvidence) (allVertices g)
      t = tableFromWeights $ V.fromList $ zip v (repeat 1.0)
  in
  Sampler t (gibbsInitValue evidence g) (gibbsGraphSampler g)  gibbsSampling

-- | Gibbs sampling
gibbsMCMCSampler :: (Factor f, FunctorWithVertex g, DirectedGraph g) 
                 => BayesianNetwork g f -- ^ Bayesian network
                 -> [DVI] -- ^ Evidence
                 -> Sampler g DVI -- ^ Sampler
gibbsMCMCSampler g evidence = 
  let isEvidence v = v `elem` (map vertex evidence)
      v = filter (not . isEvidence) (allVertices g)
      t = tableFromWeights $ V.fromList $ zip v (repeat 1.0)
  in
  Sampler t (gibbsInitValue evidence g) (gibbsMCMCGraphSampler g)  gibbsSampling

-- | Gibbs sampling for continuous network 
continuousMCMCSampler :: ContinuousNetwork -- ^ Bayesian network
                      -> [CVI] -- ^ Evidence
                      -> Sampler DirectedSG CVI-- ^ Sampler
continuousMCMCSampler g evidence = 
  let isEvidence v = v `elem` (map vertex evidence)
      v = filter (not . isEvidence) (allVertices g)
      t = tableFromWeights $ V.fromList $ zip v (repeat 1.0)
  in
  Sampler t (continuousInitValue evidence g) (gibbsContinuousMCMCGraphSampler g)  gibbsSampling

-- | Ancestral sampler which does not support evidence
discreteAncestralSampler :: (Factor f, FunctorWithVertex g, DirectedGraph g) => BayesianNetwork g f -> Sampler g DVI
discreteAncestralSampler g = 
  let nodes = topologicalOrder g
  in
  Sampler nodes (discreteInitValue [] g) (discreteSampler g)  ancestralSampling


-- | Distribution function from a factor
-- When we compute the distribution P(x | abc...), abc are given by the graph
-- but x is varied so we must not take its value fromthe graph.
-- So, x is an argument. 
distributionFromFactor :: (DirectedGraph g,Factor f) => f -> DistributionF g DV DVI 
distributionFromFactor f = 
    let vars@(main:_) = factorVariables f
    in 
    Distri (return $ BoundedSupport main) $ \current -> do 
      sample <- ask
      let currentV = vertex main
          sample' = fromJust . changeVertexValue currentV current $ sample
      return $ factorValue f (variableInstantiations sample' vars)

-- | Distribution function from  markov blanket but not normalized
distributionFromMarkovBlanket :: (DirectedGraph g,Factor f) => BayesianNetwork g f -> f -> DistributionF g DV DVI 
distributionFromMarkovBlanket g f = 
  let vars@(main:_) = factorVariables f
    in 
    Distri (return $ BoundedSupport main) $ \current -> do 
      sample <- ask
      let currentV = vertex main 
          sample' = fromJust . changeVertexValue currentV current $ sample
          children = childrenNodes sample currentV
          childrenF = map (fromJust . vertexValue g) children 
          values = map (\aFactor -> factorValue aFactor (variableInstantiations sample' (factorVariables aFactor))) (f:childrenF)
      return $ product values

continuousDistributionFromMarkovBlanket :: ContinuousNetwork 
                                        -> Distri 
                                        -> DistributionF DirectedSG (Double,Double) CVI
continuousDistributionFromMarkovBlanket g (D cv d@(Distri b distri)) = Distri b $ \current -> do 
      sample <- ask
      let currentV = vertex cv 
          children = childrenNodes sample currentV
          dist (D _ d) = d
          childrenF = map (dist . fromJust . vertexValue g) children 
      local (fromJust . changeVertexValue currentV current) $ do
        s' <- ask
        values <- mapM (uncurry distributionOnNetwork) ((d,currentV):zip childrenF children)
        return $ product $ values

-- | Value of distribution functions for all range values
forAllRange :: Graph g => Sample g DVI -> DistributionF g DV DVI -> [Double] 
forAllRange g (Distri bound r) = 
  let range = allInstantiationsForOneVariable theBound 
  in 
  map (flip runReader g . r) range
 where 
  BoundedSupport theBound = runReader bound g


-- | Value of distribution on Sample
distributionValue :: Graph g => DistributionF g bounds inst -> Sample g inst -> inst -> Double 
distributionValue (Distri bound r) g v = runReader (r v) g


-- | Probability distribution of A given some values for the parents in P(A | parents)
distribution :: Graph g => Sample g DVI -> DistributionF g DV DVI  -> CondensedTableV Int
distribution g f = 
  let notNullProba (i,p) = p /= 0.0
      values = forAllRange g f
  in
  tableFromWeights $ V.fromList  $ filter notNullProba $ zip [0..] values

-- | Get the values of factor variables on the current instantiation
variableInstantiations :: (DirectedGraph g, BayesianVariable v) => Sample g a -> [v] -> [a]
variableInstantiations g vars = 
  let vertices = map vertex vars
  in
  map (fromJust . vertexValue g) vertices

-- | Force the value of the vertex if it is part of the evidence otherwise
-- use the function
withEvidence :: (Monad m, BayesianVariable v, BayesianVariable vb) => [v] -> vb -> (vb -> m v) -> m v
withEvidence e v f = 
  if (vertex v) `elem` (map vertex e) 
    then 
      return . head . filter (\x -> vertex v == vertex x) $ e
    else 
      f v


-- | Null instantiations (min bound for each variable)
nullInstantiation :: Factor f => [DVI] -> Vertex -> f -> DVI
nullInstantiation evidence v f  = setDVValue (factorMainVariable f) 0 

-- | Random instantiation for a variable 
randomInstantiation :: Factor f => GenIO -> [DVI] -> Vertex -> f -> IO DVI 
randomInstantiation gen evidence vertex f = withEvidence evidence vertex $ \v -> do 
  let main = factorMainVariable f
      t = tableFromWeights $ V.fromList $ zip [0..dimension main - 1] (repeat 1.0)
  r <- genFromTable t gen 
  return (setDVValue main r)

continuousRandomInstantiation :: GenIO -> [CVI] -> Vertex -> Distri -> IO CVI 
continuousRandomInstantiation gen evidence vertex (D cv (Distri b _)) = withEvidence evidence vertex $ \v -> do 
   value <- uniformR (-10.0,10.0) gen :: IO Double
   return (cv =: value)

-- | Update one vertex in the current sample
updateOneSample :: (DirectedGraph g,Factor f) => GenIO -> Vertex -> f -> (Sample g DVI -> IO DVI)
updateOneSample gen v f = \s -> do
    let d = distributionFromFactor f
    newVal <- genFromTable (distribution s d) gen 
    let v = factorMainVariable f 
    return (setDVValue v newVal)

-- | Update one vertex in the current sample using gibbs transitions
gibbsUpdateOneSample :: (DirectedGraph g,Factor f) => BayesianNetwork g f -> GenIO -> Vertex -> f -> (Sample g DVI -> IO DVI)
gibbsUpdateOneSample g gen v f = 
    let d = distributionFromMarkovBlanket g f
        v = factorMainVariable f 
    in 
    \s -> do
        newVal <- genFromTable (distribution s d) gen 
        return (setDVValue v newVal)


class SamplingBounds bound inter | bound -> inter where 
  samplingBounds :: Num inter => bound -> (inter,inter)

class SamplingBounds bound inter => SampleGeneration var inst bound inter | var -> inst, var -> bound, var -> inter where
  generateSample :: Instantiable var inter inst => Sample g inst -> DistributionF g bound inst -> GenIO -> var -> IO inst

instance SamplingBounds DV Int where 
  samplingBounds dv = (0, dimension dv - 1)

instance SampleGeneration DV DVI DV Int where
  generateSample s (Distri b _) gen var = do 
      let theBound = runReader b s
      case theBound of 
        BoundedSupport s -> do
            let (sa,sb) = samplingBounds s
            u <- uniformR (sa,sb) gen
            return $ setDVValue var u
        Unbounded v -> error "DVI are bounded"

instance SamplingBounds (Double,Double) Double where 
  samplingBounds (sa,sb) = (sa,sb)

instance SampleGeneration CV CVI (Double,Double) Double where
  generateSample s (Distri b _) gen var = do 
     let theBound = runReader b s
     case theBound of 
       BoundedSupport s -> do 
                let (sa,sb) = samplingBounds s
                u <- uniformR (sa,sb) gen
                return $ var =: u
       Unbounded sigma -> do 
                n <- normal 0.0 sigma gen
                return $ var =: n

gibbsMCMCUpdateOneSample :: (DirectedGraph g,Factor f) 
                         => BayesianNetwork g f 
                         -> GenIO 
                         -> Vertex 
                         -> f 
                         -> (Sample g DVI -> IO DVI)
gibbsMCMCUpdateOneSample g gen v f = 
  let d = distributionFromMarkovBlanket g f
      main = factorMainVariable f
      v = vertex main
  in 
  _gibbsMCMCUpdateOneSample d gen v main

gibbsContinuousMCMCUpdateOneSample :: ContinuousNetwork 
                                   -> GenIO 
                                   -> Vertex 
                                   -> Distri
                                   -> (ContinuousSample -> IO CVI)
gibbsContinuousMCMCUpdateOneSample g gen v distri@(D cv _) = 
  let d = continuousDistributionFromMarkovBlanket g distri
      v = vertex cv
  in 
  _gibbsMCMCUpdateOneSample d gen v cv

-- | Generate a sample using MCMC method.
-- | Update one vertex in the current sample using gibbs transitions
_gibbsMCMCUpdateOneSample :: (DirectedGraph g,BayesianVariable var, Instantiable var inter inst, SampleGeneration var inst bound inter) 
                          => DistributionF g bound inst
                          -> GenIO
                          -> Vertex
                          -> var
                          -> (Sample g inst -> IO inst)
_gibbsMCMCUpdateOneSample d@(Distri _ _)  gen v var = \s -> do
        newSample <- generateSample s d gen var
        let oldSample = fromJust . vertexValue s $ v
            oldProbaValue = distributionValue d s oldSample
            newProbaValue = distributionValue d s newSample
        if oldProbaValue == 0.0
          then 
            return newSample 
          else 
            let r = newProbaValue / oldProbaValue 
            in
            if (r >= 1.0) 
              then 
                return newSample 
              else do 
                z <- uniformR (0.0, 1.0)  gen
                if z < r 
                  then 
                    return newSample 
                  else 
                    return oldSample



-- | Sampler graph where each vertex know how to update its value
discreteSampler :: (FunctorWithVertex g, Factor f, DirectedGraph g) => BayesianNetwork g f -> GenIO -> SamplerGraph g DVI
discreteSampler g gen = fmapWithVertex (updateOneSample gen) g

-- | Sampler graph where each vertex know how to update its value
gibbsGraphSampler :: (FunctorWithVertex g, Factor f, DirectedGraph g) => BayesianNetwork g f -> GenIO -> SamplerGraph g DVI
gibbsGraphSampler g gen = fmapWithVertex (gibbsUpdateOneSample g gen) g

-- | Sampler graph where each vertex know how to update its value
gibbsMCMCGraphSampler :: (FunctorWithVertex g, Factor f, DirectedGraph g) => BayesianNetwork g f -> GenIO -> SamplerGraph g DVI
gibbsMCMCGraphSampler g gen = fmapWithVertex (gibbsMCMCUpdateOneSample g gen) g

gibbsContinuousMCMCGraphSampler :: ContinuousNetwork
                                -> GenIO 
                                -> SamplerGraph DirectedSG CVI
gibbsContinuousMCMCGraphSampler g gen = fmapWithVertex (gibbsContinuousMCMCUpdateOneSample g gen) g

-- | Start value
discreteInitValue :: (FunctorWithVertex g, Factor f, DirectedGraph g) => [DVI] -> BayesianNetwork g f -> GenIO -> IO (Sample g DVI) 
discreteInitValue evidences g gen = fmapWithVertexM (randomInstantiation gen evidences) g

-- | Start value
gibbsInitValue :: (FunctorWithVertex g, Factor f, DirectedGraph g) => [DVI] -> BayesianNetwork g f -> GenIO -> IO (Sample g DVI) 
gibbsInitValue = discreteInitValue

continuousInitValue ::[CVI] -> ContinuousNetwork -> GenIO -> IO ContinuousSample
continuousInitValue evidences g gen = fmapWithVertexM (continuousRandomInstantiation gen evidences) g
