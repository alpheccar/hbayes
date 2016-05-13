{-# LANGUAGE ViewPatterns #-}
{- | Examples of influence diagrams

An influence diagram is an extension of a Bayesian network with can be used to solve some decision problems.
In an influence diagram, there are two new kind of nodes : decision nodes and utility nodes.

Solving an influence diagram means determining the strategies for each decision variable that will maximize the average utility.

There must be an ordering of the decision variables : a path through all the decisions.

A decision variable can depend on other past decisions and probabilistic nodes. In the later case, the variable of 
the probabilistic node is assumed to be observed before the decision is taken. So, the decision is only trying to 
maximize the average utility based on what has not been observed (the future and some past probabilistic variables).

A probabilistic node can depend on other probabilistic nodes (like in a Bayesian network) and decision nodes.

An utility is a leaf of the graph.

/Example graph/

Building an influence diagram is done like for a Bayesian network : by using the right monad.

@
import Bayes.InfluenceDiagram 
studentSimple = snd . 'runID' $ do
@

Then, you create the different nodes of the graph:

@
    e <- 'decisionNode' \"E\" ('t' :: E)
    uc <- 'utilityNode' \"UC\"
    ub <- 'utilityNode' \"UB\"
    i <- 'chance' "I" ('t' :: I)
    pr <- 'chance' "P" ('t' :: Bool)
@

The types used above are:

@
data E = Dont | Do deriving(Eq,Enum,Bounded)
data I = Low | Average | High deriving(Eq,Enum,Bounded)
@

Then, you need to define the dependencies and the numerical values. For probabilistic nodes, it is done like
for Bayesian network:

@
    cpt pr ['d' e] ~~ [1-0.0000001,1 - 0.001,0.0000001, 0.001]
    cpt i ['p' pr, 'd' e] ~~ [0.2,0.1,0.01,0.01,0.6,0.5,0.04,0.04,0.2,0.4,0.95,0.95]
@

The list may contain decision variables of type 'DEV' and probabilistic variables of type 'DV' or 'TDV'. So, the 
functions 'p' an 'd' are used for the embedding in the heterogenous list.

For decision nodes, the method is similar but with two differences : The first decision may depend on nothing (just on the assumed future).
And there are no values to define for a decision variable since the goal of the influence diagram is to compute them.

@
    'decision' e 'noDependencies'
@

For the utility nodes, it is similar to probabilistic nodes. You define the dependencies and the numerical values:

@
    'utility' uc [e] ~~ [0,-50000]
    'utility' ub [i] ~~ [100000,200000,500000]
@
  
Once the influence diagram is defined, you can solve it:

@
    'solveInfluenceDiagram' studentSimple
@

The result of this function is the solution : the decision strategies. You may want to display also the original
graph to see to which node are corresponding the vertex numbers.

/Policy Network/

You can transform a solved influence diagram into a policy network : a Bayesian network where decision variables have been replaced
with probabilistic variables where the conditional probability table is containing 1 for a choice of variables corresponding
to the decision and 0 otherwise.

@
    let l = 'solveInfluenceDiagram' student
        g = 'policyNetwork' l student
    print g 
    'printGraphValues' g
@ 

-}
module Bayes.Examples.Influence(
    -- * Influence diagrams
      exampleID
    , student
    , studentSimple
    , market 
    -- * Variables for some networks
    , studentDecisionVars
    , studentSimpleDecisionVar
    -- * Tests for the networks
    , theTest
    , policyTest
    , marketTest
    ) where 

import Bayes.InfluenceDiagram 
import Bayes(printGraphValues)
import Bayes.Factor(forAllInstantiations,dv,instantiationValue,DVSet(..))

-- | Very simple example with one decision node
exampleID :: InfluenceDiagram
exampleID = snd . runID $ do 
    a <- chance "A" (t :: Bool)
    d1 <- decisionNode "D" (t :: Bool)
    u <- utilityNode "U"

    proba a ~~ [0.8,0.1]
    decision d1 [a]
    utility u [d d1,p a] ~~ [1,10,8,2]
    return ()

data E = Dont | Do deriving(Eq,Enum,Bounded)
data I = Low | Average | High deriving(Eq,Enum,Bounded)
data S = Found | DontFound deriving(Eq,Enum,Bounded)

studentSimpleDecisionVar :: DEV 

-- | Student network as found in the book by Barber
studentSimple :: InfluenceDiagram
(studentSimpleDecisionVar,studentSimple) = runID $ do 
    e <- decisionNode "E" (t :: E)

    uc <- utilityNode "UC"
    ub <- utilityNode "UB"

    i <- chance "I" (t :: I)
    pr <- chance "P" (t :: Bool)

    cpt pr [d e] ~~ [1-0.0000001,1 - 0.001,0.0000001, 0.001]

    cpt i [p pr, d e] ~~ [0.2,0.1,0.01,0.01,0.6,0.5,0.04,0.04,0.2,0.4,0.95,0.95]
    decision e noDependencies

    utility uc [e] ~~ [0,-50000]
    utility ub [i] ~~ [100000,200000,500000]
    return e

-- | Solve the influences diagrams for the both student network.
-- Also displays each network
theTest = do
    print studentSimple
    printGraphValues studentSimple
    putStrLn "RESULT"
    print $ solveInfluenceDiagram studentSimple
    putStrLn "----"
    print student
    printGraphValues student
    putStrLn "RESULT"
    print $ solveInfluenceDiagram student


-- | Solve the influence diagram 'student' and convert it into
-- a policy network
policyTest = do 
    print student 
    printGraphValues student
    let l = solveInfluenceDiagram student
        g = policyNetwork l student
    print g 
    printGraphValues g

studentDecisionVars :: (DEV,TDV Bool,DEV)   

-- | Student network as found in the book by Barber
student :: InfluenceDiagram
(studentDecisionVars,student) = runID $ do 
    e <- decisionNode "E" (t :: E)
    s <- decisionNode "S" (t :: S)

    uc <- utilityNode "UC"
    ub <- utilityNode "UB"
    us <- utilityNode "US"

    pr <- chance "P" (t :: Bool)
    i <- chance "I" (t :: I)

    cpt pr [d e] ~~ [1-0.0000001,1 - 0.001,0.0000001, 0.001]

    cpt i [p pr, d s] ~~ [0.2,0.1,0.05,0.005, 0.6,0.5,0.15,0.005,0.2,0.4,0.8,0.99]
    decision s [pr]
    decision e noDependencies

    utility uc [e] ~~ [0,-50000]
    utility ub [i] ~~ [100000,200000,500000]
    utility us [s] ~~ [0,-200000]
    return (e,pr,s)


{- 

Test with a market network
    
-}
data F = Forecast | NoForecast deriving(Eq,Enum,Bounded)
data IN = Choice0 | Choice1 | Choice2 deriving(Eq,Enum,Bounded)
data EF = Up | Flat | Down deriving(Eq,Enum,Bounded)

genValues :: ([DVI] -> Double) -> [DV] -> [Double]
genValues f l = [f x | x <- forAllInstantiations (DVSet l)]

e :: Enum a => DVI -> a
e = toEnum . instantiationValue 

vf :: [DVI] -> (EF,F,EF)
vf [a,b,c] = (e a, e b, e c)
vf _ = (toEnum 0, toEnum 0, toEnum 0)

uf :: [DVI] -> (EF,IN,F)
uf [a,b,c] = (e a, e b, e c)
uf _ = (toEnum 0, toEnum 0, toEnum 0)

getForecastUtility (uf -> (Up, Choice0, Forecast)) = 1500
getForecastUtility (uf -> (Up, Choice0, NoForecast)) = 1500
getForecastUtility (uf -> (Up, Choice1, Forecast)) = 1000
getForecastUtility (uf -> (Up, Choice1, NoForecast)) = 1000
getForecastUtility (uf -> (Up, Choice2, Forecast)) = 500
getForecastUtility (uf -> (Up, Choice2, NoForecast)) = 500

getForecastUtility (uf -> (Flat, Choice0, Forecast)) = 100
getForecastUtility (uf -> (Flat, Choice0, NoForecast)) = 100
getForecastUtility (uf -> (Flat, Choice1, Forecast)) = 200
getForecastUtility (uf -> (Flat, Choice1, NoForecast)) = 200
getForecastUtility (uf -> (Flat, Choice2, Forecast)) = 500
getForecastUtility (uf -> (Flat, Choice2, NoForecast)) = 500

getForecastUtility (uf -> (Down, Choice0, Forecast)) = -1000
getForecastUtility (uf -> (Down, Choice0, NoForecast)) = -1000
getForecastUtility (uf -> (Down, Choice1, Forecast)) = -100
getForecastUtility (uf -> (Down, Choice1, NoForecast)) = -100
getForecastUtility (uf -> (Down, Choice2, Forecast)) = 500
getForecastUtility (uf -> (Down, Choice2, NoForecast)) = 500

getForecastProba (vf -> (Up,Forecast,Up)) = 0.8
getForecastProba (vf -> (Up,Forecast,Flat)) = 0.15
getForecastProba (vf -> (Up,Forecast,Down)) = 0.2
getForecastProba (vf -> (Up,NoForecast,Up)) = 0.33
getForecastProba (vf -> (Up,NoForecast,Flat)) = 0.33
getForecastProba (vf -> (Up,NoForecast,Down)) = 0.33

getForecastProba (vf -> (Flat,Forecast,Up)) = 0.1
getForecastProba (vf -> (Flat,Forecast,Flat)) = 0.7
getForecastProba (vf -> (Flat,Forecast,Down)) = 0.2
getForecastProba (vf -> (Flat,NoForecast,Up)) = 0.33
getForecastProba (vf -> (Flat,NoForecast,Flat)) = 0.33
getForecastProba (vf -> (Flat,NoForecast,Down)) = 0.33

getForecastProba (vf -> (Down,Forecast,Up)) = 0.1
getForecastProba (vf -> (Down,Forecast,Flat)) = 0.15
getForecastProba (vf -> (Down,Forecast,Down)) = 0.6
getForecastProba (vf -> (Down,NoForecast,Up)) = 0.33
getForecastProba (vf -> (Down,NoForecast,Flat)) = 0.33
getForecastProba (vf -> (Down,NoForecast,Down)) = 0.33

-- | Market diagram
market :: InfluenceDiagram
market = snd . runID $ do 
    o <- decisionNode "Obtain Forecast" (t :: F)
    i <- decisionNode "Investment" (t :: IN)

    ef <- chance "Economy Forecast" (t :: EF)
    ma <- chance "Market Activity" (t :: EF)

    u <- utilityNode "Payoff"

    proba ma ~~ [0.5,0.3,0.2]
    decision o noDependencies 
    decision i [d o,p ef]
    cpt ef [d o, p ma] ~~ (genValues getForecastProba [dv ef, dv o, dv ma])
    utility u [p ma, d i, d o] ~~ (genValues getForecastUtility [dv ma, dv i, dv o])
    return ()

-- | Solve the 'market' influence diagram
marketTest = do 
    print market 
    printGraphValues market
    let l = solveInfluenceDiagram market
    print l 
