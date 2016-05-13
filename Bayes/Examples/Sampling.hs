{- | Example of sampling

Two samplers are availables : the 'discreteAncestralSampler' and the 'gibbsSampler'.
Only the 'gibbsSampler' can be used with evidence.

In this example, we have a very simple network.

@
    simple :: (['TDV' Bool],'SBN' 'CPT')
    simple = 'runBN' $ do 
        a <- 'variable' \"a\" ('t' :: Bool)
        b <- 'variable' \"b\" ('t' :: Bool) 
--        
        'proba' a '~~' [0.4,0.6]
        'cpt' b [a] '~~' [0.8,0.2,0.2,0.8]
--    
        return [a,b]
@

This network is representing a sensor b. We observe the value of b and we want to infer the value of a.

We use the 'gibbsSampler' for this with an initial period of 200 samples which are dropped. The 'gibbsSampler' is
generate a stream of samples. From this stream, we need to compute a probability distribution. For this, we use
the 'samplingHistograms' histogram function which is generating a list : the probability values of each vertex.

@
    let (vars\@[a,b],exampleG) = simple
    n <- 'runSampling' 5000 200 ('gibbsSampler' exampleG [b '=:' True])
    let h = 'samplingHistograms' n
    print $ h
@

Then, we compare this result with the exact one we get with a junction tree.

@
    let jt = 'createJunctionTree' 'nodeComparisonForTriangulation' exampleG
        jt' = 'changeEvidence' [b '=:' True] jt
    mapM_ (\x -> print . 'posterior' jt' $ [x]) vars
@

We can also use the 'discreteAncestralSampler' to compute the posterior but it is not supporting the use of evidence in this
version. The syntax is similar.

@
    n <- 'runSampling' 500 ('discreteAncestralSampler' exampleG)
@

-}
module Bayes.Examples.Sampling(
    -- * Test function
	  testSampling
) where

import Bayes.Sampling 
import System.Random.MWC.CondensedTable
import qualified Data.Vector as V
import Bayes.Factor
import Bayes
import Bayes.FactorElimination
import Data.Maybe(fromJust)
import Bayes.BayesianNetwork
import Bayes.Factor.CPT
import Statistics.Sample.Histogram


simple :: ([TDV Bool],SBN CPT)
simple = runBN $ do 
    a <- variable "a" (t :: Bool)
    b <- variable "b" (t :: Bool) 
    
    proba a ~~ [0.4,0.6]
    cpt b [a] ~~ [0.8,0.2,0.2,0.8]

    return [a,b]


testSampling = do
    let (vars@[a,b],exampleG) = simple
        jt = createJunctionTree nodeComparisonForTriangulation exampleG
    --n <- runSampling 500 (discreteAncestralSampler exampleG)
    putStrLn "The bayesian network"
    print exampleG
    putStrLn "\nThe values of the Bayesian network"
    printGraphValues exampleG

    n <- runSampling 5000 200 (gibbsSampler exampleG [b =: True])
    let h = samplingHistograms 2 n
    putStrLn "\nThe result of the sampling"
    print $ h

    n <- runSampling 5000 200 (gibbsMCMCSampler exampleG [b =: True])
    let h = samplingHistograms 2 n
    putStrLn "\nThe result of the sampling with MCMC"
    print $ h

    let jt' = changeEvidence [b =: True] jt
    putStrLn "\nThe result of the junction tree inference"
    mapM_ (\x -> print . posterior jt' $ [x]) vars
