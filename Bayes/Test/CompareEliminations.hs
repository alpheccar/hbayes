{- | A comparison of variable elimination and factor elimination on a simple graph.

It is a non regression test.

-}
module Bayes.Test.CompareEliminations(
   compareVariableFactor
 , compareFactorChange
 ) where

import Test.HUnit.Lang(assertFailure)

import Bayes.Examples(example,exampleWithFactorChange)
import Bayes.Factor
import Bayes
import Bayes.VariableElimination
import Bayes.FactorElimination
import Bayes.Factor.CPT
import Data.Maybe(fromJust)

compareFactors :: String -> Maybe CPT -> CPT -> IO ()
compareFactors s Nothing _ = assertFailure s
compareFactors s (Just a) b = 
    if a `isomorphicFactor` b 
        then 
            return () 
        else 
            assertFailure s

compareFactorChange :: IO () 
compareFactorChange = do 
    let ([winter,sprinkler,rain,wet,road,roadandrain],exampleG) = (map dv . fst $ example,snd example)
        (_,exampleGF) = exampleWithFactorChange 
        theNewFactor = fromJust $ factorWithVariables [wet,sprinkler,rain] [1,0.2,0.1,0.05,0,0.8,0.9,0.95]
        exampleGF' = changeFactor theNewFactor exampleGF
        compareG s a b = compareFactors s (Just $ priorMarginal exampleGF' a b) (priorMarginal exampleG a b)
    compareG "PRIOR FOR RAIN" [winter,sprinkler,wet,road,roadandrain] [rain]
    compareG "PRIOR FOR WINTER" [sprinkler,wet,road,rain,roadandrain] [winter]
    compareG "PRIOR FOR SPRINKLER" [winter,wet,road,rain,roadandrain] [sprinkler]
    compareG "PRIOR FOR WET" [winter,sprinkler,road,rain,roadandrain] [wet]
    compareG "PRIOR FOR ROAD" [winter,sprinkler,wet,rain,roadandrain] [road]

    -- Now we test that the junction tree factors can be changed and the result will be right.
    -- We first create a wrong junction tree using the wrong graph (where factor values are wrong)
    let jt' = createJunctionTree nodeComparisonForTriangulation exampleGF 
    -- We then update the values to the right ones by using the right factor
        jt = changeFactor theNewFactor jt'

    compareFactors "PRIOR FOR RAIN" (posterior jt [rain]) (priorMarginal exampleG [winter,sprinkler,wet,road,roadandrain] [rain])

    let jt1 = changeEvidence [wet =: True] jt 
        jt2 = changeEvidence [wet =: True, sprinkler =: True] jt1 

    compareFactors "POSTERIOR RAIN FOR WET" (posterior jt1 [rain]) 
         (posteriorMarginal exampleG [winter,sprinkler,wet,road,roadandrain] [rain]  [wet =: True])
    compareFactors "POSTERIOR RAIN FOR WET" (posterior jt2 [rain]) 
         (posteriorMarginal exampleG [winter,sprinkler,wet,road,roadandrain] [rain]  [wet =: True, sprinkler =: True])

    compareFactors "PRIOR FOR WINTER" (posterior jt [winter]) (priorMarginal exampleG [sprinkler,wet,road,rain,roadandrain] [winter])
    compareFactors "PRIOR FOR SPRINKLER" (posterior jt [sprinkler]) (priorMarginal exampleG [winter,wet,road,rain,roadandrain] [sprinkler])
    compareFactors "PRIOR FOR WET" (posterior jt [wet]) (priorMarginal exampleG [winter,sprinkler,road,rain,roadandrain] [wet])
    compareFactors "PRIOR FOR ROAD" (posterior jt [road]) (priorMarginal exampleG [winter,sprinkler,wet,rain,roadandrain] [road])


-- | Compare that variable elemination and factor elimination are giving
-- similar results on a simple example
compareVariableFactor :: IO ()
compareVariableFactor = do 
    let ([winter,sprinkler,rain,wet,road,roadandrain],exampleG) = example
        jt = createJunctionTree nodeComparisonForTriangulation exampleG
    compareFactors "PRIOR FOR RAIN" (posterior jt [rain]) (priorMarginal exampleG [winter,sprinkler,wet,road,roadandrain] [rain])

    let jt1 = changeEvidence [wet =: True] jt 
        jt2 = changeEvidence [wet =: True, sprinkler =: True] jt1 

    compareFactors "POSTERIOR RAIN FOR WET" (posterior jt1 [rain]) 
         (posteriorMarginal exampleG [winter,sprinkler,wet,road,roadandrain] [rain]  [wet =: True])
    compareFactors "POSTERIOR RAIN FOR WET" (posterior jt2 [rain]) 
         (posteriorMarginal exampleG [winter,sprinkler,wet,road,roadandrain] [rain]  [wet =: True, sprinkler =: True])

    compareFactors "PRIOR FOR WINTER" (posterior jt [winter]) (priorMarginal exampleG [sprinkler,wet,road,rain,roadandrain] [winter])
    compareFactors "PRIOR FOR SPRINKLER" (posterior jt [sprinkler]) (priorMarginal exampleG [winter,wet,road,rain,roadandrain] [sprinkler])
    compareFactors "PRIOR FOR WET" (posterior jt [wet]) (priorMarginal exampleG [winter,sprinkler,road,rain,roadandrain] [wet])
    compareFactors "PRIOR FOR ROAD" (posterior jt [road]) (priorMarginal exampleG [winter,sprinkler,wet,rain,roadandrain] [road])

