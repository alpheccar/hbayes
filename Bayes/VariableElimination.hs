{- | Algorithms for variable elimination

-}
module Bayes.VariableElimination(
 -- * Inferences
   priorMarginal
 , posteriorMarginal
 -- * Interaction graph and elimination order
 , interactionGraph
 , degreeOrder
 , minDegreeOrder
 , minFillOrder
 , allVariables
 , marginal
 , mpemarginal
 , mpe
 , EliminationOrder
 ) where

import Bayes
import Bayes.Factor
import Data.List(minimumBy,(\\),foldl')
import Data.Maybe(fromJust)
import Data.Function(on)
import qualified Data.Map as M
import Bayes.Factor.PrivateCPT(convertToMaxFactor,CPT,MAXCPT)
import Bayes.Factor.CPT 
import Bayes.Factor.MaxCPT
import Bayes.PrivateTypes(DVISet)
import Bayes.VariableElimination.Buckets

--import Debug.Trace 

--debug s a = trace (s  ++ "\n" ++ show a ++ "\n") a



-- | Get all variables from a Bayesian Network
allVariables :: (Graph g, Factor f) 
             => BayesianNetwork g f 
             -> [DV]
allVariables g = 
  let s = allVertexValues g 
      createDV = factorMainVariable 
  in 
  map createDV s


convertToMaxCPT :: Buckets CPT -> Buckets MAXCPT 
convertToMaxCPT (Buckets e m) = Buckets e (M.map (map convertToMaxFactor) m) 



-- | Compute the prior marginal. All the variables in the
-- elimination order are conditionning variables ( p( . | conditionning variables) )
marginal :: (IsBucketItem f, Factor f)
         => [f] -- ^ Bayesian Network
         -> EliminationOrder DV -- ^ Ordering of variables to marginalize
         -> EliminationOrder DV -- ^ Ordering of remaining variables
         -> [DVI] -- ^ Assignment for some factors in variables to marginalize
         -> f
marginal lf p r assignment = 
  -- The elimintation order are the variables to eliminate.
  -- But the algorithm also needs the remaining variables
  let bucket = createBuckets lf p r
      assignmentFactors = map factorFromInstantiation assignment
      bucket' = foldl' addBucket bucket assignmentFactors
      Buckets _ resultBucket = foldl' marginalizeOneVariable bucket' p
      resultFactor = factorProduct . concat . M.elems $ resultBucket
      -- The norm is P(e) and result factor is P(Q,e)
  in
  -- We get P(Q , e)
  resultFactor
 

-- | Compute the prior marginal. All the variables in the
-- elimination order are conditionning variables ( p( . | conditionning variables) )
-- First we sum, then we maximize for the remaining variables
mpemarginal :: [CPT] -- ^ Bayesian Network
            -> EliminationOrder DV -- ^ Ordering of variables to marginalize
            -> EliminationOrder DV -- ^ Ordering of remaining variables
            -> [DVI] -- ^ Assignment for some factors in variables to marginalize
            -> MAXCPT
mpemarginal lf p r assignment = 
  -- The elimintation order are the variables to eliminate.
  -- But the algorithm also needs the remaining variables
  let bucket = createBuckets lf p r
      assignmentFactors = map factorFromInstantiation assignment
      bucket' = foldl' addBucket bucket assignmentFactors
      bucket'' = foldl' marginalizeOneVariable bucket' p
      bucketMax = convertToMaxCPT bucket''
      Buckets _ resultBucket = foldl' marginalizeOneVariable bucketMax r
      resultFactor = factorProduct  . concat . M.elems $ resultBucket
      -- The norm is P(e) and result factor is P(Q,e)
  in
  -- We get P(Q , e)
  resultFactor
 

-- | Most Probable Explanation (or Maximum A Posteriori estimator)
-- when restricted to a subest of variables in output
mpe :: (Graph g, BayesianDiscreteVariable dva, BayesianDiscreteVariable dvb) 
    => BayesianNetwork g CPT -- ^ Bayesian network defining the factors
    -> EliminationOrder dva -- ^ Ordering of variables to sum out (should contain evidence variables)
    -> EliminationOrder dvb -- ^ Ordering of remaining variables (to maximize)
    -> [DVI] -- ^ Assignment
    -> [DVISet] -- ^ MPE or MAP instantiation
mpe g someP someR assignment = 
    let p = map dv someP
        r = map dv someR
        s = allVertexValues g 
        resultFactor = mpemarginal s p r assignment
    in 
    mpeInstantiations (resultFactor)

posteriorMarginal :: (Graph g, IsBucketItem f, Factor f,Show f, BayesianDiscreteVariable dva, BayesianDiscreteVariable dvb) 
                  => BayesianNetwork g f -- ^ Bayesian Network
                  -> EliminationOrder dva -- ^ Ordering of variables to marginzalie
                  -> EliminationOrder dvb-- ^ Ordering of remaining variables
                  -> [DVI] -- ^ Assignment for some factors in variables to marginalize
                  -> f
posteriorMarginal g someP someR assignment = 
  let p = map dv someP 
      r = map dv someR
      s = allVertexValues g 
      resultFactor = marginal s p r assignment
      norm = factorNorm resultFactor
  in
  -- We get P(Q | e)
  resultFactor `factorDivide` norm 

-- | Compute the prior marginal. All the variables in the
-- elimination order are conditionning variables ( p( . | conditionning variables) )
priorMarginal :: (Graph g, IsBucketItem f, Factor f,Show f, BayesianDiscreteVariable dva, BayesianDiscreteVariable dvb) 
              => BayesianNetwork g f -- ^ Bayesian Network
              -> EliminationOrder dva-- ^ Ordering of variables to marginalize
              -> EliminationOrder dvb-- ^ Ordering of remaining to keep in result
              -> f
priorMarginal g someEA someEB = 
  let ea = map dv someEA 
      eb = map dv someEB
      s = allVertexValues g 
      resultFactor = marginal s ea eb []
      norm = factorNorm resultFactor
  in
  -- We get P(Q | e)
  resultFactor `factorDivide` norm    

-- | Compute the interaction graph of the BayesianNetwork
interactionGraph :: (FoldableWithVertex g,Factor f, UndirectedGraph g')
                 => BayesianNetwork g f
                 -> g' () DV
interactionGraph g = 
  foldrWithVertex addFactor emptyGraph g 
 where
  addFactor vertex factor graph = 
    let allvars = factorVariables factor
        edges = [(x,y) | x <- allvars, y <- allvars , x /= y]
        addNewEdge g (va,vb)  = 
          let g' = addVertex (variableVertex vb) vb . addVertex (variableVertex va) va $ g 
          in
          addEdge (edge (variableVertex va) (variableVertex vb)) () $ g'
    in 
    foldl' addNewEdge graph edges

-- | Number of neighbors for a variable in the bayesian network
nbNeighbors :: UndirectedSG () DV 
            -> DV 
            -> Int 
nbNeighbors g dv = 
  let r = fromJust $ neighbors g (variableVertex dv)
  in 
  length r

-- | Number of missing links between the neighbors of the graph
nbMissingLinks :: UndirectedSG () DV  
               -> DV 
               -> Int 
nbMissingLinks g dv = 
  let r = fromJust $ neighbors g (variableVertex dv)
      edges = [(x,y) | x <- r, y <- r , x /= y, not (isLinkedWithAnEdge g x y)]
  in 
  length edges

-- | Compute the degree order of an elimination order
degreeOrder :: (FoldableWithVertex g, Factor f, Graph g)
            => BayesianNetwork g f
            -> EliminationOrder DV 
            -> Int 
degreeOrder g p =
  let  ig = interactionGraph g :: UndirectedSG () DV
       (_,w) = foldl' processVariable (ig,0) p 
  in 
  w 
 where 
  addAnEdge g (va,vb)  = addEdge (edge va vb) () g
  processVariable (g,w) bdv  = 
    let r = fromJust $ neighbors g (variableVertex bdv)
        nbNeighbors = length r
        edges = [(x,y) | x <- r, y <- r , x /= y, not (isLinkedWithAnEdge g x y)]
        g' = removeVertex (variableVertex bdv) (foldl' addAnEdge g edges)
    in
    if nbNeighbors > w 
      then 
        (g',nbNeighbors) 
      else 
        (g',w)
 
-- | Find an elimination order minimizing a metric
eliminationOrderForMetric :: (Graph g, Factor f, FoldableWithVertex g, UndirectedGraph g')
                          => (g' () DV -> DV -> Int)
                          -> BayesianNetwork g f 
                          -> EliminationOrder DV  
eliminationOrderForMetric metric g = 
  let ig = interactionGraph g
      s = allVertexValues ig
      getOptimalNode _ [] = []
      getOptimalNode g l = 
        let (optimalNode,_) = minimumBy (compare `on` snd) . map (\v -> (v,metric g v)) $ l
            g' = removeVertex (variableVertex optimalNode) g
        in 
        optimalNode : getOptimalNode g' (l \\ [optimalNode])
  in 
    getOptimalNode ig s

-- | Elimination order minimizing the degree
minDegreeOrder :: (Graph g, Factor f, FoldableWithVertex g)
               => BayesianNetwork g f 
               -> EliminationOrder DV  
minDegreeOrder = eliminationOrderForMetric nbNeighbors

-- | Elimination order minimizing the filling
minFillOrder :: (Graph g, Factor f, FoldableWithVertex g)
               => BayesianNetwork g f 
               -> EliminationOrder DV  
minFillOrder = eliminationOrderForMetric nbMissingLinks