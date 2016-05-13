{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | Testing of the implementation.

-}
module Bayes.Test (
    runTests
    ) where
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit(testCase)
import Bayes.Test.CompareEliminations(compareVariableFactor,compareFactorChange)

import Bayes(testEdgeRemoval_prop,testVertexRemoval_prop)
import Bayes.Factor.CPT(testProductProject_prop,testScale_prop,testProjectCommut_prop,testScalarProduct_prop,testProjectionToScalar_prop,testAssocProduct_prop)
import Bayes.FactorElimination(junctionTreeProperty_prop,junctionTreeAllClusters_prop)
import Bayes.PrivateTypes(instantiationProp)
import Bayes.Test.InfluencePatterns(testStudentDecisions)
#ifdef LOCAL
import Bayes.Test.ReferencePatterns(compareAsiaReference,compareCancerReference,comparePokerReference,compareFarmReference,compareMpeAsia,testFileExport)
#else 
import Bayes.Test.ReferencePatterns(testFileExport)
#endif

-- | Run all the tests
runTests = defaultMain tests

tests = [
          testGroup "Graph" [
                testProperty "Edge Removal" testEdgeRemoval_prop,
                testProperty "Vertex Removal" testVertexRemoval_prop
            ]
        , testGroup "Factor" [
                testProperty "Factor scaling and norm" testScale_prop,
                testProperty "Product / Project" testProductProject_prop,
                testProperty "Commutativity of project" testProjectCommut_prop,
                testProperty "Product with scalar factor" testScalarProduct_prop,
                testProperty "Test projection to scalar" testProjectionToScalar_prop,
                testProperty "Test associativity of factor" testAssocProduct_prop,
                testCase "Test factors can be changed in containers" compareFactorChange
            ]
        , testGroup "Junction Tree" [
                testProperty "Test the junction tree property" junctionTreeProperty_prop,
                testCase "Test variable elimination == factor elimination" compareVariableFactor,
                testProperty "Test all clusters are included in the junction tree" junctionTreeAllClusters_prop
            ]
        , testGroup "Misc functions" [
                testProperty "Instantiation from multiindex" instantiationProp,
                testCase "Test import/export of bayesian network and junction tree" testFileExport
            ]
        , testGroup "Influence Diagrams" [
                testCase "Test with reference patterns" testStudentDecisions
            ]
#ifdef LOCAL
        , testGroup "Reference patterns" [ 
                testCase "Asia reference pattern" compareAsiaReference,
                testCase "Cancer reference pattern" compareCancerReference,
                testCase "Poker reference pattern" comparePokerReference,
                testCase "Farm reference pattern" compareFarmReference,
                testCase "Test MPE and MAP with Asia network" compareMpeAsia
        ]
#endif

    ]


