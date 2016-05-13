{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- Bayesian Networks with continuous variables

-}
module Bayes.Continuous(
    -- * Types 
    -- ** For the graph description
      CNMonad(..)
    , CV
    , DN
    , VariableName(..)
    , BayesianVariable(..)
    -- ** For the sampling 
    , Distri 
    , ContinuousNetwork(..) 
    , ContinuousSample(..)
    , InstantiationValue(..)
    , CVI 
    -- * Network creation functions
    , uniform 
    , normal
    , beta
    , beta'
    , exponential
    , gammaD
    , execCN
    , runCN 
    , evalCN
    -- * Sampling functions
    , runSampling 
    , continuousMCMCSampler
    , (=:)
    -- * Result statistics
    , histogram
    , samplingHistograms 
    ) where

import Bayes.Sampling
import Bayes
import Data.Monoid
import Control.Applicative
import Bayes.PrivateTypes
import Bayes.Network(NetworkMonad(..),runGraph,execGraph,evalGraph)
import Control.Monad.State.Strict
import Data.Maybe(fromJust)
import Control.Monad
import Control.Monad.Reader 
import Bayes.Sampling(runSampling, samplingHistograms,gibbsMCMCSampler, ContinuousNetwork(..), ContinuousSample(..)) 
import Math.Gamma(gamma)

-- | The Bayesian monad
type CNMonad a = GraphMonad DirectedSG () Distri a


instance Monoid (DistributionSupport (Double,Double)) where 
    mempty = BoundedSupport (0.0,0.0)
    (BoundedSupport (xa,ya)) `mappend` (BoundedSupport (xb,yb)) = BoundedSupport (min xa xb, max ya yb)
    (BoundedSupport _) `mappend` (Unbounded a) = Unbounded a 
    (Unbounded a) `mappend` (BoundedSupport _) = Unbounded a 
    (Unbounded a) `mappend` (Unbounded b) = Unbounded (a+b)

newtype RN = RN (Reader (Sample DirectedSG CVI) Double) 

instance Num RN where 
   (RN a) + (RN b) = RN $ liftA2 (+) a b
   (RN a) - (RN b) = RN $ liftA2 (-) a b
   (RN a) * (RN b) = RN $ liftA2 (*) a b
   abs (RN a) = RN $ liftA abs a 
   signum (RN a) = RN $ liftA signum a 
   fromInteger a = RN (return (fromInteger a))

instance Fractional RN where 
   (RN a) / (RN b) = RN $ liftA2 (/) a b
   fromRational a = RN (return (fromRational a))

instance Floating RN where 
  pi = RN (return pi)
  exp (RN b) = RN $ liftA exp b
  sqrt (RN b) = RN $ liftA sqrt b 
  log (RN b) = RN $ liftA log b
  sin (RN b) = RN $ liftA sin b
  cos (RN b) = RN $ liftA cos b
  tan (RN b) = RN $ liftA tan b
  asin (RN b) = RN $ liftA asin b 
  atan (RN b) = RN $ liftA atan b 
  acos (RN b) = RN $ liftA acos b 
  sinh (RN b) = RN $ liftA sinh b 
  tanh (RN b) = RN $ liftA tanh b 
  cosh (RN b) = RN $ liftA cosh b 
  asinh (RN b) = RN $ liftA asinh b 
  atanh (RN b) = RN $ liftA atanh b 
  acosh (RN b) = RN $ liftA acosh b 
  (**) (RN xb) (RN yb) = RN $ liftA2 (**) xb yb
  (logBase) (RN xb) (RN yb) = RN $ liftA2 logBase xb yb

-- | An expression which can be a constant, variable or formula.
-- In case it is a variable, it can be used as a 'BayesianVariable'
-- or instantiated as an 'Instantiable' type.
-- Otherwise you'll get an error
data DN = DN !([CV]) !RN

instance BayesianVariable DN where 
    vertex (DN [] _) = error "No vertex for this expression"
    vertex (DN (v:[]) _) = vertex v 
    vertex (DN l _) = error "Too many vertices for this expression"

dependencies :: DN -> [CV]
dependencies (DN l _) = l 

instance Instantiable DN Double CVI where 
  (=:) (DN [] _) x = error "No variable is related to this expression"
  (=:) (DN (a:[]) _) x = a =: x
  (=:) (DN l _) x = error "Too many variables are related to this expression"

value :: DN -> Reader (Sample DirectedSG CVI) Double 
value (DN _ (RN v)) = v

instance Num DN where 
    (DN da na) + (DN db nb) = DN (da `mappend`db) (na + nb)
    (DN da na) - (DN db nb) = DN (da `mappend`db) (na - nb)
    (DN da na) * (DN db nb) = DN (da `mappend`db) (na * nb)
    abs (DN a b) = DN a (abs b)
    signum (DN a b) = DN a (signum b)
    fromInteger i = DN [] (fromInteger i)

instance Fractional DN where
    (DN da na) / (DN db nb) = DN (da `mappend`db) (na / nb)
    fromRational i = DN [] (fromRational i)

instance Floating DN where 
  pi = DN [] pi
  exp (DN a b) = DN a (exp b) 
  sqrt (DN a b) = DN a (sqrt b) 
  log (DN a b) = DN a (log b) 
  sin (DN a b) = DN a (sin b) 
  cos (DN a b) = DN a (cos b) 
  tan (DN a b) = DN a (tan b) 
  asin (DN a b) = DN a (asin b) 
  atan (DN a b) = DN a (atan b) 
  acos (DN a b) = DN a (acos b) 
  sinh (DN a b) = DN a (sinh b) 
  tanh (DN a b) = DN a (tanh b) 
  cosh (DN a b) = DN a (cosh b) 
  asinh (DN a b) = DN a (asinh b) 
  atanh (DN a b) = DN a (atanh b) 
  acosh (DN a b) = DN a (acosh b) 
  (**) (DN xa xb) (DN ya yb) = DN (xa `mappend` ya) (xb ** yb)
  (logBase) (DN xa xb) (DN ya yb) = DN (xa `mappend` ya) (xb ** yb)



{-

Graph creation

-}

-- | This class is used to simplify the network description. Variable
-- names can be optional. In that later case, () must be used instead of a name.
class VariableName m where 
    mkVariable :: m -> CNMonad CV 

instance VariableName String where 
    mkVariable = variable 

instance VariableName () where 
    mkVariable _ = unamedVariable

-- | Create a new named Bayesian variable if not found.
-- Otherwise, return the found one.
addVariableIfNotFound :: String -> CNMonad Vertex
addVariableIfNotFound vertexName = graphNode vertexName (D (CV (Vertex 0)) (Distri (return mempty) (\s -> return 0.0)))

-- | Create a new unamed variable
unamedVariable :: CNMonad CV
unamedVariable = do 
  ((namemap,count),g) <- get 
  let vertexName = "unamed" ++ show count
  va <- addVariableIfNotFound vertexName
  return (CV va)


whenJust Nothing _ = return ()
whenJust (Just i) f = f i >> return ()

-- | Set the node of a bayesian network under creation
setBayesianNode :: CV -> Distri -> CNMonad ()
setBayesianNode (CV v) newValue = do
  (aux,oldGraph) <- get
  let newGraph = changeVertexValue v newValue oldGraph
 
  whenJust newGraph $ \nvm -> do
     put $! (aux, nvm)

-- | Create an edge between two vertex of the Bayesian network
(<--) :: CV -> CV -> CNMonad ()
(CV va) <-- (CV vb) = newEdge vb va mempty



node :: CV -> DN
node v = DN [v] (RN $ do 
    r <- ask 
    return . instantiationValue . fromJust . vertexValue r $ (vertex v)
    )

-- | Define a Bayesian variable (name and bounds)
variable :: String -- ^ Variable name
         -> CNMonad CV
variable name = do
  va <- addVariableIfNotFound name
  return (CV va)

cpt :: CV -> [CV] -> CNMonad CV
cpt node conditions = do
  mapM_ (node <--) (reverse conditions)
  return node

{-

Distributions

-}

-- | Exponential distribution 
exponential :: VariableName s 
            => s 
            -> DN 
            -> CNMonad DN 
exponential s na = do 
    v <- mkVariable s
    let la = dependencies na 
    cpt v la 
    let bound = do 
           lambda <- value na
           return (Unbounded (1.0 / lambda))
        result = D v (Distri bound $ \inst -> do 
                    lambda <- value na
                    let x = instantiationValue inst 
                    if x < 0
                      then 
                        return 0.0 
                      else 
                        return $ lambda * exp(-lambda * x)
                    )
    setBayesianNode v result 
    return (node v)

-- | Gamma distribution 
gammaD :: VariableName s 
       => s 
       -> DN -- ^ r
       -> DN -- ^ lambda
       -> CNMonad DN 
gammaD s rm lambdam = do 
    v <- mkVariable s
    let la = dependencies rm 
        lb = dependencies lambdam 
    let l = la ++ lb
    cpt v l
    let bound = do 
            lambda <- value lambdam 
            r <- value rm
            return . Unbounded $ r/lambda/lambda
        result = D v (Distri bound $ \inst -> do 
                    lambda <- value lambdam
                    r <- value rm
                    let x = instantiationValue inst 
                    if x < 0 
                      then 
                        return 0.0 
                      else 
                        return $ lambda**r * x**(r-1)*exp(-lambda*x)/gamma r
                    )
    setBayesianNode v result 
    return (node v)

-- | Beta distribution 
beta :: VariableName s 
     => s 
     -> DN 
     -> DN 
     -> CNMonad DN 
beta s na nb = do 
    v <- mkVariable s
    let la = dependencies na 
        lb = dependencies nb 
    let l = la ++ lb
    cpt v l
    let bound = return (BoundedSupport (0.0,0.1) )
        result = D v (Distri bound $ \inst -> do 
                    a <- value na
                    b <- value nb
                    let x = instantiationValue inst 
                    return $ gamma(a+b)/gamma(a)/gamma(b)*x**(a-1)*(1-x)**(b-1)
                    )
    setBayesianNode v result 
    return (node v)

-- | Beta' distribution 
beta' :: VariableName s 
     => s 
     -> DN 
     -> DN 
     -> CNMonad DN 
beta' s na nb = do 
    v <- mkVariable s
    let la = dependencies na 
        lb = dependencies nb 
    let l = la ++ lb
    cpt v l
    let bound = do 
              a <- value na
              b <- value nb
              let std = a * (a + b - 1) -- Not really the standard deviation but related to it
                                        -- enough for generating new samples
              return (Unbounded std)
        result = D v (Distri bound $ \inst -> do 
                    a <- value na
                    b <- value nb
                    let x = instantiationValue inst 
                    if x <= 0 
                      then 
                        return 0.0
                      else
                        return $ gamma(a+b)/gamma(a)/gamma(b)*x**(a-1)*(1+x)**(-a-b)
                    )
    setBayesianNode v result 
    return (node v)

-- | Uniform dstribution
uniform :: VariableName s 
        => s -- ^ Variable name
        -> DN -- ^ Min bound
        -> DN -- ^ Max bound
        -> CNMonad DN
uniform s na nb = do 
    v <- mkVariable s
    let la = dependencies na 
        lb = dependencies nb 
    let l = la ++ lb
    cpt v l
    let bound = do
         a <- value na 
         b <- value nb 
         return (BoundedSupport (a,b) )
    let result = D v (Distri bound $ \inst -> do 
                    a <- value na
                    b <- value nb
                    let d = 1.0 / (b - a)
                    if instantiationValue inst < a || instantiationValue inst > b then return 0.0 else return d)
    setBayesianNode v result 
    return (node v)


-- | Normal distribution
normal :: VariableName s 
       => s -- ^ Variable name
       -> DN -- ^ Average
       -> DN -- ^ Standard deviation
       -> CNMonad DN
normal s mean std = do
    v <- mkVariable s
    let la = dependencies mean 
        lb = dependencies std 
    let l = la ++ lb
    cpt v l
    let bound = do 
         s <- value std 
         return (Unbounded s )
    let result = D v (Distri bound $ \inst -> do 
                    m <- value mean
                    s <- value std
                    let x = instantiationValue inst 
                    return (1.0 / (sqrt (2 * pi)*s) * exp(-(x-m)*(x-m)/(2*s*s))
                           ))
    setBayesianNode v result 
    return (node v)

{-
 
Graph creation from the Monad.  

-}

-- | Create a  network using the simple graph implementation
-- The initialized nodes are replaced by the value.
-- Returns the monad values and the built graph.
runCN :: CNMonad a -> (a,ContinuousNetwork)
runCN = runGraph

-- | Create a  network but only returns the monad value.
-- Mainly used for testing.
execCN :: CNMonad a -> ContinuousNetwork
execCN = execGraph

-- | Create a bayesian network but only returns the monad value.
-- Mainly used for testing.
evalCN :: CNMonad a -> a
evalCN = evalGraph
