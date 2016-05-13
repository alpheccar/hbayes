{- | Tutorial explaining how to make infereces with the library.

Thus tutorial is using examples from the module "Bayes.Examples". Please,
refer to this module for documentation about how the example bayesian networks are
created or loaded.

/Inferences/

The function 'inferencesOnStandardNetwork' is showing how to use variable elimination
and factor elimination to make inferences.

First, the 'example' is loaded to make its variables and its bayesian network available:

@
    let ([winter,sprinkler,rain,wet,road],exampleG) = example
@

Then, we compute a prior marginal. Prior means that no evidence is used. A bayesian
network is a factorisation of a distribution P(A B C ...). If you want to know the
probability of only A, you need to sum out the other variables to eliminate them and get
P(A). To compute this prior marginal using variable elimnation, you need to give an elimination
order. The complexity of the computation is depending on the elimination order chosen.

For instance, if you want to compute the prior probability of rain, you can write:

@
    'priorMarginal' exampleG [winter,sprinkler,wet,road] [rain] 
@

Now, if you have observed that the grass is wet and want to take into account thios observation
to compute the posterior probability of rain (after observation):

@
    'posteriorMarginal' exampleG [winter,sprinkler,wet,road] [rain]  [wet '=:' True]
@ 

If you want to combine several observations:

@
    'posteriorMarginal' exampleG [winter,sprinkler,wet,road] [rain]  [wet '=:' True, sprinkler '=:' True]
@

There are several problems with variable elimination:

 * You have to specify an elimination order 

 * If you want to compute another marginal (for instance probability of winter), you have
 to recompute everything.

But, there exists another category of elimination algorithms based upon factor elimination. 
They require the creation of an auxiliary data structure : the junction tree.

This tree is then used for computing all marginals (without having to recompute everything).
The junction tree is equivalent to giving an elimination order.

So, the previous examples can also be computed with factor elimination. First, the 
junction tree must created:

@
    let jt = 'createJunctionTree' 'nodeComparisonForTriangulation' exampleG
@

The junction tree being equivalent to an elimination order, the order chosen will
depend on a cost function. In the previous example, the cost function 'nodeComparisonForTriangulation'
is used. Other cost functions may be introduced in a futute version of this library.

Once the junction tree has been computd, it can be used to compute several marginals:

@
    'posterior' jt [rain]
@

The function is called posterior and will compute posterior only when solme evidence has
been introduced into the tree. Otherwise it is computing a prior.

To set evidence, you need to update the junction tree with new evidence:

@
    let jt' = 'updateEvidence' [wet '=:' True] jt 
    'posterior' jt' [rain]
@

If you want to compute the posterior for a combination of variables, you have two possibilities : either going back to the
variable elimination methods. Or, introduce new nodes in the network to represent the query.

It is easily done through the new 'logical' function when building the Bayesian graph.

Once you have a node to represent a complex query, you can use it to compute a posterior. For instance, in the rain example,
there is a new variable:

@
    roadandrain <- 'variable' \"rain and slippery road\" (t :: Bool)
    'logical' roadandrain ((rain '.==.' True) '.&.' (road '.==.' True))
@

This variable is representing the assertion : rain True AND slippery road True. This variable can be used
 to answer different queries, like for instance:

@
    let jt4 = 'changeEvidence' [wet '=:' True] jt 
    print \"Posterior Marginal : probability of rain and road slippery if grass wet\"
    let m = 'posterior' jt4 [roadandrain]
    print m
--
    let jt5 = 'changeEvidence' [wet '=:' True, sprinkler '=:' False] jt 
    print \"Posterior Marginal : probability of rain and road slippery if grass wet and srinkler not used\"
    let m = 'posterior' jt5 [roadandrain]
@

/Inferences with an imported network/

There is a slight additional difficulty with imported networks : you need
to create new data type to be able to set evidence.

For instance, in the cancer network there is a Coma variable with levels Present or Absent.
When imported, those levels are imported as number. But, the evidence API in this library is
requiring enumerations.

So, you need to create a 'Coma' type:

@
    data Coma = Present | Absent deriving(Eq,Enum,Bounded)
@

and check that 'Present' is corresponding to the level 0 in the imported network.

Once this datatype is created, you can easily use the cancer network. First we load
the network and import the discrete variables of type 'DV' from the names of the nodes in the
network (not the label of the nodes).

@
    print \"CANCER NETWORK\"
    (varmap,cancer) <- 'exampleImport'
    print cancer
    let [varA,varB,varC,varE] = fromJust $ mapM (flip Map.lookup varmap) [\"A\",\"B\",\"C\",\"E\"]
@

To avoid any errors with the future queries, some imported variables can be transformed into typed variables:

@
    varD = 'tdv' (fromJust $ Map.lookup \"D\" varmap) :: 'TDV' Coma
@

Once the variables are available, you can create the junction tree and start making inferences:

@
    let jtcancer = 'createJunctionTree' 'nodeComparisonForTriangulation' cancer
--
    mapM_ (\x -> putStrLn (show x) >> (print . 'posterior' jtcancer $ [x])) [varA,varB,varC,varE]
--
    print \"UPDATED EVIDENCE\"
    let jtcancer' = 'updateEvidence' [varD '=:' Present] jtcancer 
    mapM_ (\x -> putStrLn (show x) >> (print . 'posterior' jtcancer' $ [x])) [varA,varB,varC,varE]
@

The '=:' operator will check that the assignment is type compatible because varD is a typed discrete variable of type 'TDV' Coma.

/MPE inferences/

It is possible to compute the Most Probable Explanation for a set of observation. The syntax is very similar to the
posterior computation with variable elimination:

@
    let m = 'mpe' exampleG [wet,road] [winter,sprinkler,rain,roadandrain] [wet '=:' True, road '=:' True]
@

The first list of variables (which should containg the evidence variables) is summed out.
The second list of variables is used to maximize the probability.
Both lists should contain all variables of the Bayesian network and are defining an elimination order.

The result of the mpe functions is a list of instantiations. The result is easier to read when the type information is
reintroduced. It can be done with the 'tdvi' function:

@
    let typedResult = map (map 'tdvi') m :: [[('TDV' Bool,Bool)]]
@

In this example, all variables are boolean ones.

/ Soft Evidence /

Soft evidence is more complex to handle since new node have to be added to the graph.
And the node factor has to be changed when the node evidence is changed.

Here is how you could do it. First you load an example graph containg a soft evidence node created with 'softEvidence'.

@
inferencesWithSoftEvidence = do 
    let ((a,seNode),exampleG) = 'exampleSoftEvidence' 
@

Then, you create the junction tree as usual and force an hard evidence on the soft evidence node.

@
        jt = 'createJunctionTree' 'nodeComparisonForTriangulation' exampleG
        jt' = 'changeEvidence' [seNode '=:' True] jt
@

This junction tree cannot be used because the soft evidence node created in 'exampleSoftEvidence' has a 
probability table which is meaningless. You need to update the probability table for a given soft evidence.
You create a new factor for this:

@
        theNewFactor x = fromJust $ 'se' seNode a x -- x % success for the sensor
@

This new factor, can then be used to do inference with different soft evidences.

@
    print \"Sensor 90%\"
    print $ posterior ('changeFactor' (theNewFactor 0.9) jt') [a]
--
    print \"Sensor 50%\"
    print $ posterior ('changeFactor' (theNewFactor 0.5) jt') [a]
--
    print \"Sensor 10%\"
    print $ posterior ('changeFactor' (theNewFactor 0.1) jt') [a]
@

-}
module Bayes.Examples.Tutorial(
    -- * Tests with the standard network 
      inferencesOnStandardNetwork
    , mpeStandardNetwork
    , inferencesWithSoftEvidence
#ifndef LOCAL
    -- * Tests with the cancer network
    , inferencesOnCancerNetwork
    , Coma(..)
#endif
#ifdef LOCAL
    , miscDiabete
#endif
    , miscTest
    , logicalTest
	) where 

import Bayes.Factor
import Bayes
import Bayes.VariableElimination
#ifndef LOCAL
import Bayes.Examples(example, exampleLogical,exampleSoftEvidence,exampleJunction,exampleImport,exampleDiabete, exampleAsia, examplePoker, exampleFarm,examplePerso,anyExample)
#else 
import Bayes.Examples(example, exampleLogical,exampleSoftEvidence,exampleJunction,exampleDiabete, exampleAsia, examplePoker, exampleFarm,examplePerso,anyExample)
#endif
import Bayes.FactorElimination
import Data.Function(on)
import qualified Data.Map as Map
import Data.Maybe(fromJust,mapMaybe)
import System.Exit(exitSuccess)
import qualified Data.List as L((\\))
import Bayes.BayesianNetwork(se)


#ifdef LOCAL
miscDiabete = do 
  (varmap,perso) <- exampleDiabete
  let jtperso = createJunctionTree nodeComparisonForTriangulation perso
      cho0 = fromJust . Map.lookup "cho_0" $ varmap
  print $ posterior jtperso [cho0]
#endif

miscTest s = do 
  (varmap,perso) <- anyExample s
  let names = Map.keys varmap
      l =  mapMaybe (flip Map.lookup varmap) names
      jtperso = createJunctionTree nodeComparisonForTriangulation perso
  print perso
  print jtperso
  print "FACTOR ELIMINATION"
  let post (v,name) = do 
        putStrLn name 
        print $ posterior jtperso [v]
  mapM_ post  (zip l names)

  print "VARIABLE ELIMINATION"
  let prior (v,name) = do 
        putStrLn name 
        print $ priorMarginal perso (l L.\\ [v]) [v]
  mapM_ prior (zip l names)


#ifndef LOCAL

-- | Type defined to set the evidence on the Coma variable
-- from the cancer network.
data Coma = Present | Absent deriving(Eq,Enum,Bounded)

-- | Inferences with the cancer network
inferencesOnCancerNetwork = do 
    print "CANCER NETWORK"
    (varmap,cancer) <- exampleImport
    print cancer
    let [varA,varB,varC,varE] = fromJust $ mapM (flip Map.lookup varmap) ["A","B","C","E"]
        jtcancer = createJunctionTree nodeComparisonForTriangulation cancer
        varD = tdv (fromJust $ Map.lookup "D" varmap) :: TDV Coma
    mapM_ (\x -> putStrLn (show x) >> (print . posterior jtcancer $ [x])) [varA,varB,varC,varE]

    print "UPDATED EVIDENCE : Coma present"
    let jtcancer' = changeEvidence [varD =: Present] jtcancer 
    mapM_ (\x -> putStrLn (show x) >> (print . posterior jtcancer' $ [x])) [varA,varB,varC,varE]

    print "UPDATED EVIDENCE : Coma absent"
    let jtcancer' = changeEvidence [varD =: Absent] jtcancer 
    mapM_ (\x -> putStrLn (show x) >> (print . posterior jtcancer' $ [x])) [varA,varB,varC,varE]
#endif 

-- | Display of factors generated by the logical keyword
logicalTest = do 
    let ([a,b,notV,andV,orV],g) = exampleLogical
        fnot = fromJust $ vertexValue g (vertex notV)
        fand = fromJust $ vertexValue g (vertex andV)
        for = fromJust $ vertexValue g (vertex orV)
    print fnot 
    print fand 
    print for

-- | Inferences with soft evidence
inferencesWithSoftEvidence = do 
    let ((a,seNode),exampleG) = exampleSoftEvidence 
        jt = createJunctionTree nodeComparisonForTriangulation exampleG
        theNewFactor x = fromJust $ se seNode a x -- x % success for the sensor
        jt' = changeEvidence [seNode =: True] jt
    print "Sensor 90%"
    print $ posterior (changeFactor (theNewFactor 0.9) jt') [a]

    print "Sensor 50%"
    print $ posterior (changeFactor (theNewFactor 0.5) jt') [a]

    print "Sensor 10%"
    print $ posterior (changeFactor (theNewFactor 0.1) jt') [a]

-- | Inferences with the standard network
inferencesOnStandardNetwork = do
    let ([winter,sprinkler,rain,wet,road,roadandrain],exampleG) = example

    print exampleG
    putStrLn ""
    print "VARIABLE ELIMINATION"
    putStrLn ""
    print "Prior Marginal : probability of rain"
    let m = priorMarginal exampleG [winter,sprinkler,wet,road] [rain] 
    print m
    putStrLn ""

    print "Posterior Marginal : probability of rain if grass wet"
    let m = posteriorMarginal exampleG [winter,sprinkler,wet,road] [rain]  [wet =: True]
    print m
    putStrLn ""

    print "Posterior Marginal : probability of rain if grass wet and sprinkler used"
    let m = posteriorMarginal exampleG [winter,sprinkler,wet,road] [rain]  [wet =: True, sprinkler =: True]
    print m
    putStrLn ""

    let jt = createJunctionTree nodeComparisonForTriangulation exampleG
    print jt
    displayTreeValues jt
    putStrLn ""
    print "FACTOR ELIMINATION"
    putStrLn ""
    print "Prior Marginal : probability of rain"
    let m = posterior jt [rain]
    print m
    putStrLn ""

    let jt' = changeEvidence [wet =: True] jt 

    print "Posterior Marginal : probability of rain if grass wet"
    let m = posterior jt' [rain]
    print m
    putStrLn ""

    let jt'' = changeEvidence [] jt'
    print "Prior Marginal : probability of rain"
    let m = posterior jt [rain]
    print m
    putStrLn ""

    let jt3 = changeEvidence [wet =: True, sprinkler =: True] jt'
    print "Posterior Marginal : probability of rain if grass wet and sprinkler used"
    let m = posterior jt3 [rain]
    print m
    putStrLn ""

    let jt4 = changeEvidence [wet =: True] jt 
    print "Posterior Marginal : probability of rain and road slippery if grass wet"
    let m = posterior jt4 [roadandrain]
    print m
    putStrLn ""

    let jt5 = changeEvidence [wet =: True, sprinkler =: False] jt 
    print "Posterior Marginal : probability of rain and road slippery if grass wet and srinkler not used"
    let m = posterior jt5 [roadandrain]
    print m
    putStrLn ""

    return ()


-- | Most likely explanation on standard network
mpeStandardNetwork = do
    let ([winter,sprinkler,rain,wet,road,roadandrain],exampleG) = example

    print exampleG
    print "Most likely explanation if grass wet and road slippery"
    let m = mpe exampleG [wet,road] [winter,sprinkler,rain,roadandrain] [wet =: True, road =: True]
        typedResult = map (map tdvi) m :: [[(TDV Bool,Bool)]]
    print typedResult
    putStrLn ""
    let m = mpe exampleG [wet,road,roadandrain,winter] [sprinkler,rain] [wet =: True, road =: True]
        typedResult = map (map tdvi) m :: [[(TDV Bool,Bool)]]
    print typedResult
    putStrLn ""
