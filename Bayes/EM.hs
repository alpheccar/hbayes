{- | Expectation / Maximization to learn Bayesian network values

-}
module Bayes.EM( 
    -- * Learning function
      learnEM
    ) where 

import Bayes
import Bayes.Factor 
import Bayes.Sampling 
import Bayes.Factor.CPT
import Bayes.FactorElimination
import Data.Maybe(fromJust)


sumG :: (FunctorWithVertex g, Graph g) 
     => Sample g (CPT,CPT) 
     -> Sample g (CPT,CPT) 
     -> Sample g (CPT,CPT)
sumG ga gb = 
  let sumNode vertex (xa,xb) = 
        let (ya,yb) = fromJust . vertexValue gb $ vertex 
        in 
        (cptSum [xa,ya], cptSum [xb,yb])
  in 
  fmapWithVertex sumNode ga 

divideG :: Vertex 
        -> (CPT,CPT) 
        -> CPT
divideG _ (a,b) = cptDivide a b 

-- | Learn network values from samples using the expectation / maximization algorithm.
learnEM :: (FunctorWithVertex g, NamedGraph g, FoldableWithVertex g, DirectedGraph g) 
        => [[DVI]] -- ^ Samples
        -> BayesianNetwork g CPT -- ^ Start network
        -> BayesianNetwork g CPT -- ^ Network with new values learnt from the samples
learnEM samples startG = 
    let jt = createJunctionTree nodeComparisonForTriangulation startG 
        results = map (computeSample startG jt) samples
    in 
    fmapWithVertex divideG (foldl1 sumG results)

computeSample :: (FunctorWithVertex g, Graph g) 
              => BayesianNetwork g CPT 
              -> JunctionTree CPT 
              -> [DVI] 
              -> Sample g (CPT,CPT)
computeSample startG jt s = fmapWithVertex (computeNode startG jt s) startG

computeNode :: Graph g 
            => BayesianNetwork g CPT 
            -> JunctionTree CPT 
            -> [DVI] 
            -> Vertex 
            -> CPT 
            -> (CPT,CPT)
computeNode g jt samples vertex _ = 
    let jt' = changeEvidence samples jt
        f@(main:l) = factorVariables (fromJust . vertexValue g $ vertex)
        a = fromJust $ posterior jt' f 
        b = if null l then factorFromScalar 1.0 else fromJust $ posterior jt' l 
    in 
    (a,b)
