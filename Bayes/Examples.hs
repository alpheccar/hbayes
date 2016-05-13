{- | Examples of networks

/Creating a simple network/

The 'example' function is the typical example.
It is using the monad 'BNMonad'. The goal of this monad is to offer
a way of describing the network which is natural.

There are only three functions to understand inside the monad:

  * 'variable' to create a discrete variable of type 'DV'. Creating a discrete
  variable is using a 'Bounded' and 'Enum' type like for instance 'Bool'.

  * 'proba' to define the probability P(A) of a variable A

  * 'cpt'  to define the conditional probability table P(A | BC)

It is important to understand how the values are organized. If you define
P( wet | sprinkler road) then you have to give the values in the order:

@
wet=False, sprinkler=False, road=False
wet=False, sprinkler=False, road=True
wet=False, sprinkler=True, road=False
wet=False, sprinkler=True, road=True
@

Finally, don't forget to return the discrete variables at the end of your network
construction because those variables are used for making inferences.

@
example :: (['TDV' Bool],'SBN' 'CPT')
example = 'runBN' $ do 
    winter <- 'variable' \"winter\" (t :: Bool)
    sprinkler <- 'variable' \"sprinkler\" (t :: Bool) 
    wet <- 'variable' \"wet grass\" (t :: Bool) 
    rain <- 'variable' \"rain\" (t :: Bool) 
    road <- 'variable' \"slippery road\" (t :: Bool) 
--
    'proba' winter ~~ [0.4,0.6]
    'cpt' sprinkler [winter] ~~ [0.25,0.8,0.75,0.2]
    'cpt' rain [winter] ~~ [0.9,0.2,0.1,0.8]
    'cpt' wet [sprinkler,rain] ~~ [1,0.2,0.1,0.05,0,0.8,0.9,0.95]
    'cpt' road [rain] ~~ [1,0.3,0,0.7]
    return [winter,sprinkler,rain,wet,road]
@

By default, all variables are typed ('TDV' Bool). 'TDV' means Typed Discrete Variable.

In case you are mixing several types, you'll need to remove the type
to build the 'cpt' since the list can't be heterogeneous. Just use 'dv' for this. It will convert the variable into the 
type 'DV' of untyped discrete variable.

/Creating truth tables/

In practise, it is easy to compute the posterior of a variable because it is always possible
to find a cluster containing the variable in the junction tree. But, it is more difficult
to compute the posterior of a logical assertion or just a conjunction of assertions.

If a query is likely to be done often, then it may be a good idea to add a new node 
to the Bayesian network to represent this query. So, some functions to create truth tables are provided.

@
exampleLogical :: (['TDV' Bool], 'SBN' 'CPT')
exampleLogical = 'runBN' $ do 
    a <- 'variable' \"a\" (t :: Bool)
    b <- 'variable' \"b\" (t :: Bool)
    notV <- 'variable' \"notV\" (t :: Bool)
    andV <- 'variable' \"andV\" (t :: Bool)
    orV <- 'variable' \"orV\" (t :: Bool)
    let ta = a '.==.' True 
        tb = b '.==.' True
    'logical' notV (('.!.') ta)
    'logical' andV (ta '.&.' tb)
    'logical' orV (ta '.|.' tb)
    return $ [a,b,notV,andV,orV]
@

In the previous example, we force a type on the discrete variables 'DV' to avoid futur errors
in the instantiations. It is done through the 'tdv' function.

But, it is also possible to use the untyped variables and write:

@
    'logical' andV ((a '.==.' True) '.&.' (b '.==.' True))
@

The goal of a Bayesian network is to factorize a big probability table because otherwise the algorithms
can't process it. So, of course it is not a good idea to represent a complex logical assertion with a huge
probability table. So, the 'logical' keyword should only be used to build small tables.

If you need to encode a complex logical assertion, use 'logical' several times to build a network representing
the assertion instead of building just one node to represent it.

/Noisy OR/

The Noisy OR is a combination of logical tables (OR) and conditional probability tables which is often used
during modeling to avoid generating big conditional probability tables.

It is easy to use:

@
    no <- 'noisyOR' [(a,0.1),(b,0.2),(c,0.3)] 
@

Each probability is the probability that a given variable has no effect (so is inhibited in the OR).

/Importing a network from a Hugin file/

The 'exampleImport' function can be used to import a file in Hugin format.
Only a subset of the format is supported.
The function will return a mapping from node names to Discrete Variables 'DV'.
The node name is used and not the node's label.
The function is also returning a simple bayesian network 'SBN' using 'CPT'
as factors.

The implementation is using 'getDataFileName' to find the path of the
test pattern installed by cabal.

@
exampleImport :: IO (Map.Map String 'DV','SBN' 'CPT')
exampleImport = do 
    path <- 'getDataFileName' \"cancer.net\"
    r <- 'importBayesianGraph' path
    return ('runBN' $ fromJust r)
@

-}
module Bayes.Examples(
   example
 , exampleJunction
 , exampleWithFactorChange
 , exampleSoftEvidence
#ifndef LOCAL
 , exampleImport
#endif
 , exampleDiabete
 , exampleAsia
 , examplePoker
 , exampleFarm
 , examplePerso
 , exampleLogical
 , testJunction
 , anyExample
 ) where 

import Bayes
import Bayes.Factor
import Bayes.ImportExport.HuginNet
import Data.Maybe(fromJust)
import qualified Data.Map as Map
import System.Directory(getHomeDirectory)
import System.FilePath((</>))
import Bayes.Factor.CPT
import Bayes.BayesianNetwork

#ifndef LOCAL
import Paths_hbayes

-- | Example showing how to import a graph described into
-- a Hugin file.
exampleImport :: IO (Map.Map String DV,SBN CPT)
exampleImport = do 
    path <- getDataFileName "cancer.net"
    r <- importBayesianGraph path
    return (runBN $ fromJust r)
#endif

-- | Genereic loading functions to load some other
-- examples from the author's dropbox.
-- Those additional examples are not distributed with this package.
-- They are used only for testing and debugging purposes
genericExample :: String -> IO (Map.Map String DV,SBN CPT)
genericExample s = do 
    r <- importBayesianGraph s
    return (runBN $ fromJust r)

anyExample s = do
    h <- getHomeDirectory
    genericExample $ h </> "Dropbox/bayes_examples" </> s
   
-- | Diabete example (not provided with this package)
exampleDiabete = do 
    h <- getHomeDirectory
    genericExample $ h </> "Dropbox/bayes_examples/Diabetes.hugin"

-- | Asia example (not provided with this package)
exampleAsia = do 
    h <- getHomeDirectory
    genericExample $ h </> "Dropbox/bayes_examples/asia.net"

-- | Poker example (not provided with this package)
examplePoker = do 
    h <- getHomeDirectory
    genericExample $ h </> "Dropbox/bayes_examples/poker.net"

-- | Farm example (not provided with this package)
exampleFarm = do 
    h <- getHomeDirectory
    genericExample $ h </> "Dropbox/bayes_examples/studfarm.net"

-- | Perso example (not provided with this package)
examplePerso = do 
    h <- getHomeDirectory
    genericExample $ h </> "Dropbox/bayes_examples/mytest.net"


-- | Example of soft evidence use
exampleSoftEvidence :: ((TDV Bool,TDV Bool),SBN CPT)
exampleSoftEvidence = runBN $ do
  a <- variable "a" (t :: Bool)
  proba a ~~ [0.5,0.5]
  se <- softEvidence a 
  return (a,se) 

-- | Standard example found in many books about Bayesian Networks.
example :: ([TDV Bool],SBN CPT)
example = runBN $ do 
    winter <- variable "winter" (t :: Bool)
    sprinkler <- variable "sprinkler" (t :: Bool) 
    wet <- variable "wet grass" (t :: Bool) 
    rain <- variable "rain" (t :: Bool) 
    road <- variable "slippery road" (t :: Bool) 
    roadandrain <- variable "rain and slippery road" (t :: Bool)

    proba winter ~~ [0.4,0.6]
    cpt sprinkler [winter] ~~ [0.25,0.8,0.75,0.2]
    cpt rain [winter] ~~ [0.9,0.2,0.1,0.8]
    cpt wet [sprinkler,rain] ~~ [1,0.2,0.1,0.05,0,0.8,0.9,0.95]
    cpt road [rain] ~~ [1,0.3,0,0.7]

    logical roadandrain ((rain .==. True) .&. (road .==. True))
    return $ [winter,sprinkler,rain,wet,road,roadandrain]

-- | Standard example but with a wrong factor that is changed
-- in the tests using factor replacement functions
exampleWithFactorChange :: ([TDV Bool],SBN CPT)
exampleWithFactorChange = runBN $ do 
    winter <- variable "winter" (t :: Bool)
    sprinkler <- variable "sprinkler" (t :: Bool) 
    wet <- variable "wet grass" (t :: Bool) 
    rain <- variable "rain" (t :: Bool) 
    road <- variable "slippery road" (t :: Bool) 
    roadandrain <- variable "rain and slippery road" (t :: Bool)

    proba winter ~~ [0.4,0.6]
    cpt sprinkler [winter] ~~ [0.25,0.8,0.75,0.2]
    cpt rain [winter] ~~ [0.9,0.2,0.1,0.8]
    cpt wet [sprinkler,rain] ~~ [1,1,1,1,1,1,1,1]
    cpt road [rain] ~~ [1,0.3,0,0.7]

    logical roadandrain ((rain .==. True) .&. (road .==. True))
    return $ [winter,sprinkler,rain,wet,road,roadandrain]


exampleLogical :: ([TDV Bool], SBN CPT)
exampleLogical = runBN $ do 
    a <- variable "a" (t :: Bool)
    b <- variable "b" (t :: Bool)
    notV <- variable "notV" (t :: Bool)
    andV <- variable "andV" (t :: Bool)
    orV <- variable "orV" (t :: Bool)
    let ta = a .==. True 
        tb = b .==. True
    logical notV ((.!.) ta)
    logical andV (ta .&. tb)
    logical orV (ta .|. tb)
    return $ [a,b,notV,andV,orV]


testJunction  :: DirectedSG () Vertex
testJunction = execGraph $ do
    a <- graphNode "A" (Vertex 0) 
    b <- graphNode "B" (Vertex 1) 
    c <- graphNode "C" (Vertex 2) 
    newEdge a b () 
    newEdge a c ()

exampleJunction :: UndirectedSG () Vertex
exampleJunction = execGraph $ do 
    a <- graphNode "A" (Vertex 0) 
    b <- graphNode "B" (Vertex 1) 
    c <- graphNode "C" (Vertex 2) 
    d <- graphNode "D" (Vertex 3) 
    e <- graphNode "E" (Vertex 4) 
    f <- graphNode "F" (Vertex 5) 
    g <- graphNode "G" (Vertex 6) 
    h <- graphNode "H" (Vertex 7) 

    newEdge a b () 
    newEdge a c ()
    newEdge b d ()
    newEdge c e () 
    newEdge d e ()
    newEdge d f ()
    newEdge e f ()
    newEdge c g ()
    newEdge e h ()
    newEdge g h ()
    newEdge g e ()
    
    return ()

