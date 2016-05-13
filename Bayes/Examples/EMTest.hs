{- | Test of learning

In this example, two networks are used : 'simple' which is the reference and 'wrong' which is a wrong start.
The goal is to use test patterns to learn the right 'simple' network from 'wrong'. Only the values are learnt.
The topology of both networks is the same.

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

and 'wrong' where the probability for a is wrong.

@
wrong :: (['TDV' Bool],'SBN' 'CPT')
wrong = 'runBN' $ do 
    a <- 'variable' \"a\" ('t' :: Bool)
    b <- 'variable' \"b\" ('t' :: Bool) 
--    
    'proba' a '~~' [0.2,0.8]
    'cpt' b [a] '~~' [0.8,0.2,0.2,0.8]
--
    return [a,b]
@

So, the first thing to do is generate test patterns. We are using the 'discreteAncestralSampler' for this. This function is
generating a sequence of graphs. We are just interested in the values. So, we get the values with 'allVertexValues'.

@
generatePatterns :: IO [[DVI]]
generatePatterns = do 
    let (vars\@[a,b],exampleG) = simple
    r <- 'runSampling' 5000 0 ('discreteAncestralSampler' exampleG)
    return (map 'allVertexValues' r)
@

Once we have the data, we can try to learn the network:

@
emTest = do 
  samples <- generatePatterns 
  let (_,simpleG) = simple 
      (_,wrongG) = wrong 
  print simpleG 
  'printGraphValues' simpleG
  'printGraphValues' wrongG
--
  'printGraphValues' ('learnEM' samples wrongG)
@

First, we display the topology of the graph and the values for the reference graph and the wrong one.
Then, we use the 'learnEM' function to learn a new network from the samples. And, we print the new values to check.

-}
module Bayes.Examples.EMTest(
    -- * Test function
    emTest
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
import Bayes.EM

simple :: ([TDV Bool],SBN CPT)
simple = runBN $ do 
    a <- variable "a" (t :: Bool)
    b <- variable "b" (t :: Bool) 
    
    proba a ~~ [0.4,0.6]
    cpt b [a] ~~ [0.8,0.2,0.2,0.8]

    return [a,b]

wrong :: ([TDV Bool],SBN CPT)
wrong = runBN $ do 
    a <- variable "a" (t :: Bool)
    b <- variable "b" (t :: Bool) 
    
    proba a ~~ [0.2,0.8]
    cpt b [a] ~~ [0.8,0.2,0.2,0.8]

    return [a,b]

setJust vertex v = Just v 

generatePatterns :: IO [[DVI]]
generatePatterns = do 
    let (vars@[a,b],exampleG) = simple
    r <- runSampling 5000 0 (discreteAncestralSampler exampleG)
    return (map allVertexValues r)

emTest = do 
  samples <- generatePatterns 
  let (_,simpleG) = simple 
      (_,wrongG) = wrong 
  print simpleG 
  printGraphValues simpleG
  printGraphValues wrongG

  
  printGraphValues (learnEM samples wrongG)
 
