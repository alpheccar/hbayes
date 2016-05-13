{- | A comparison of factor elimination with reference values generated with another bayesian network software

It is a non regression test. The test patterns are not provided with this package.
So, those tests are disabled by default in the hackage version.

-}
module Bayes.Test.ReferencePatterns(
   testFileExport
#ifdef LOCAL
 , compareAsiaReference
 , compareCancerReference
 , comparePokerReference
 , compareFarmReference
 , compareMpeAsia
#endif
 ) where

import Test.HUnit.Base(assertBool)
import Data.Maybe(fromJust)
import qualified Data.Map as Map
import Bayes.Factor
import Bayes
import Bayes.FactorElimination
import Bayes.VariableElimination(mpe)
import Bayes.Examples(anyExample,example)
import Bayes.FactorElimination.JTree(root)
import Bayes.Tools(withTempFile)
import Bayes.ImportExport 
import Bayes.BayesianNetwork

value varmap jt s = 
  let v =  fromJust $ Map.lookup s varmap
    in 
    factorToList (fromJust $ posterior jt [v]) 

testWithRef varmap jt s l = assertBool s $ value varmap jt s ~=~ l
testWithRefAndPrint varmap jt s l = do
  let r = value varmap jt s 
  putStrLn $ "Computed:" ++ show r
  putStrLn $ "Reference:" ++ show l
  putStrLn ""
  assertBool s $ r ~=~ l

-- | Test that we can import / export the bayesian network, junction tree and variable map
testFileExport :: IO () 
testFileExport = do 
  let (vars,g) = example 
      vm = varMap g
      jt = createJunctionTree nodeComparisonForTriangulation g
  withTempFile $ \f -> do 
    writeNetworkToFile f g 
    g' <- readNetworkFromFile f 
    assertBool "Test graph import/export" $ g == g'
  withTempFile $ \f -> do 
    writeVariableMapAndJunctionTreeToFile f vm jt 
    (vm',jt') <- readVariableMapAndJunctionTreeToFile f 
    assertBool "Test jt import/export" $ jt == jt'
    assertBool "Test variable map import/export" $ vm == vm'
     

-- Check that the float values are equal with an accuracy < 0.01%
comparePercent :: Double -> Double -> Bool
comparePercent a b = abs (a-b) < 1e-4

(~=~) a b = and (zipWith comparePercent a b)

#ifdef LOCAL

data Positive = Yes | No deriving(Eq,Enum,Bounded,Show) 

--  | Get the name of vertices in the graph for display
rename :: NamedGraph g => g a b -> (TDV s,s) -> (String,s)
rename g = \(a,s) -> (fromJust . vertexLabel g . vertex $ a, s)

-- | Test that a MAP is not always the projection of a MPE
compareMpeAsia = do 
  (varmap,g) <- anyExample "asia.net"
  let [x,b,d,a,s,l,t,e] = map tdv . fromJust $ mapM (flip Map.lookup varmap) ["X","B","D","A","S","L","T","E"] :: [TDV Positive]
      m = mpe g [x,d] [b,a,s,l,t,e] [x =: Yes, d =: No]
      typedResult = map (map (rename g . tdvi)) m :: [[(String,Positive)]]
  assertBool "Test MPE" $ typedResult == [[("E",No),("T",No),("L",No),("S",No),("B",No),("A",No)]]
  let m = mpe g [x,d,b,l,t,e] [a,s] [x =: Yes, d =: No]
      typedResult = map (map (rename g . tdvi)) m :: [[(String,Positive)]]
  assertBool "Test MAP" $ typedResult == [[("S",Yes),("A",No)]]

compareFarmReference = do 
  (varmap,g) <- anyExample "studfarm.net"
  let jt = createJunctionTree nodeComparisonForTriangulation g
  
  assertBool "Junction Tree property" $ junctionTreeProperty jt [] (root jt)
  testWithRef varmap jt "L"  [0.01,0.99]
  testWithRef varmap jt "Ann"  [0.01,0.99]
  testWithRef varmap jt "Brian"  [0.01,0.99]
  testWithRef varmap jt "Cecily"  [0.01,0.99]
  testWithRef varmap jt "K"  [0.01,0.99]
  testWithRef varmap jt "Fred"  [0.01,0.99]
  testWithRef varmap jt "Dorothy"  [0.01,0.99]
  testWithRef varmap jt "Eric"  [0.01,0.99]
  testWithRef varmap jt "Gwenn"  [0.01,0.99]
  testWithRef varmap jt "Henry"  [0.0091,0.9909]
  testWithRef varmap jt "Irene"  [0.0099,0.9901]
  testWithRef varmap jt "John"  [0.0004,0.0087,0.9909]


comparePokerReference = do 
  (varmap,g) <- anyExample "poker.net"
  let jt = createJunctionTree nodeComparisonForTriangulation g

  assertBool "Junction Tree property" $ junctionTreeProperty jt [] (root jt)
  testWithRef varmap jt "OH0"  [0.1672, 0.0445,0.0635,0.4659,0.1694,0.0494,0.0353,0.0024,0.0024]
  testWithRef varmap jt "OH1"  [0.0265,0.0170,0.0357,0.4125,0.2633,0.1599,0.0676,0.0098,0.0077]
  testWithRef varmap jt "OH2"  [0.2472,0.0628,0.2903,0.0258,0.2526,0.0881,0.0212,0.0121]
  testWithRef varmap jt "SC"  [0.2450,0.7116,0.0435]
  testWithRef varmap jt "FC"  [0.0895,0.6988,0.0445,0.1672]
  testWithRef varmap jt "Besthand"  [0.6396,0.3604]
  testWithRef varmap jt "MH"  [0.1250,0.1250,0.1250,0.1250,0.1250,0.1250,0.1250,0.1250]


compareAsiaReference = do 
  (varmap,g) <- anyExample "asia.net"
  let jt = createJunctionTree nodeComparisonForTriangulation g

  assertBool "Junction Tree property" $ junctionTreeProperty jt [] (root jt)
  testWithRef varmap jt "A"  [0.0100, 0.9900]
  testWithRef varmap jt "S"  [0.5000, 0.5000]
  testWithRef varmap jt "T"  [0.0104, 0.9896]
  testWithRef varmap jt "L"  [0.0550, 0.9450]
  testWithRef varmap jt "B"  [0.4500, 0.5500]
  testWithRef varmap jt "E"  [0.0648, 0.9352]
  testWithRef varmap jt "X"  [0.1103, 0.8897]
  testWithRef varmap jt "D"  [0.4360, 0.5640]

-- | Type defined to set the evidence on the Coma variable
-- from the cancer network.
data Coma = Present | Absent deriving(Eq,Enum,Bounded)

compareCancerReference = do 
  (varmap,g) <- anyExample "cancer.net"
  let jt = createJunctionTree nodeComparisonForTriangulation g

  assertBool "Junction Tree property" $ junctionTreeProperty jt [] (root jt)
  testWithRef varmap jt "A"  [0.2000, 0.8000]
  testWithRef varmap jt "B"  [0.3200, 0.6800]
  testWithRef varmap jt "C"  [0.0800, 0.9200]
  testWithRef varmap jt "D"  [0.3200, 0.6800]
  testWithRef varmap jt "E"  [0.6160, 0.3840]

  let varD = fromJust $ Map.lookup "D" varmap
  let jt' = changeEvidence [varD =: Present] jt 
  testWithRef varmap jt' "A"  [0.4250, 0.5750]
  testWithRef varmap jt' "B"  [0.8000, 0.2000]
  testWithRef varmap jt' "C"  [0.2000, 0.8000]
  testWithRef varmap jt' "D"  [1.0000, 0.0000]
  testWithRef varmap jt' "E"  [0.6400, 0.3600]

  let jt'' = changeEvidence [varD =: Absent] jt'
  testWithRef varmap jt'' "A"  [0.0941, 0.9059]
  testWithRef varmap jt'' "B"  [0.0941, 0.9059]
  testWithRef varmap jt'' "C"  [0.0235, 0.9765]
  testWithRef varmap jt'' "D"  [0.0000, 1.0000]
  testWithRef varmap jt'' "E"  [0.6047, 0.3953]

#endif
