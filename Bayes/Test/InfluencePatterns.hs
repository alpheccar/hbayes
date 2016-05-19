{- | A comparison of influence diagram solution with references

-}
module Bayes.Test.InfluencePatterns(
   testStudentDecisions
 ) where

import Test.HUnit.Base(assertBool)
import Data.Maybe(fromJust)

import Bayes.Examples.Influence
import Bayes.InfluenceDiagram

testStudentDecisions = do
  let result = solveInfluenceDiagram student
      (e,pr,s) = studentDecisionVars
      l = map decisionToInstantiation result
  assertBool "Student Network" $ l == [[[e =: (0::Int)]],[[s =: (0::Int),pr =: False],[s =: (0::Int),pr =: True]]]
  let result = solveInfluenceDiagram studentSimple
      e = studentSimpleDecisionVar
      l = map decisionToInstantiation result
  assertBool "Simple Student Network" $ l == [[[ e =: (1 :: Int)]]]


