{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{- | Module for building Bayesian Networks

-}
module Bayes.BayesianNetwork(
  -- * Bayesian Monad used to ease creation of Bayesian Networks
    BNMonad
  , runBN 
  , evalBN
  , execBN
  , Distribution(..)
  -- ** Variable creation
  , variable
  , unamedVariable
  , variableWithSize
  , tdv
  , t
  -- ** Creation of conditional probability tables
  , cpt
  , proba
  , (~~)
  , softEvidence
  , se
  -- ** Creation of truth tables
  , logical 
  , (.==.)
  , (.!.)
  , (.|.)
  , (.&.)
  -- ** Noisy OR
  , noisyOR
  ) where

import Bayes
import Bayes.PrivateTypes
import Control.Monad.State.Strict
import Bayes.Factor
import Data.Maybe(fromJust)
import qualified Data.List as L(find)
import Data.List(sort,intercalate,nub)
import Bayes.Tools(minBoundForEnum,maxBoundForEnum,intValue)
import Bayes.Network 

-- | Synonym for undefined because it is clearer to use t to set the Enum bounds of a variable
t = undefined

-- | The Bayesian monad
type BNMonad g f a = NetworkMonad g () f a

-- | Initialize the values of a factor
(~~) :: (DirectedGraph g, Factor f, Distribution d, BayesianDiscreteVariable v) 
     => BNMonad g f v -- ^ Discrete variable in the graph
     -> d -- ^ List of values
     -> BNMonad g f ()
(~~) mv l = do 
  (DV v _) <- mv >>= return . dv -- This is updating the state and so the graph
  maybeNewValue <- getCpt v l
  currentValue <- getBayesianNode v
  case (currentValue, maybeNewValue) of 
    (Just c, Just n) -> initializeNodeWithValue v c n
    _ -> return ()


-- | Define a conditional probability between different variables
-- Variables are ordered like
-- FFF FFT FTF FTT TFF TFT TTF TTT
-- and same for other enumeration keeping enumeration order
-- Note that the reverse is important. We add the parents in such a way that 'ingoing'
-- will give a list of parents in the right order.
-- This order must correspond to the order of values in the initialization.
cpt :: (DirectedGraph g , BayesianDiscreteVariable v,BayesianDiscreteVariable vb) => v -> [vb] -> BNMonad g f v
cpt node conditions = do
  mapM_ ((dv node) <--) (reverse (map dv conditions))
  return node

-- | Define proba for a variable
-- Values are ordered like
-- FFF FFT FTF FTT TFF TFT TTF TTT
-- and same for other enumeration keeping enumeration order
proba :: (DirectedGraph g, BayesianDiscreteVariable v) => v -> BNMonad g f v
proba node = cpt node ([] :: [DV])

-- | Create an auxiliairy node to force soft evidence
softEvidence :: (NamedGraph g, DirectedGraph g, Factor f) 
             => TDV Bool -- ^ Variable on which we want to define Soft evidence
             -> BNMonad g f (TDV Bool) -- ^ Return a soft evidence node (for the factor encoding the soft evidence values)
             -- and an hard evidence node to activate the soft evidence observation
softEvidence d = do 
  se <- unNamedVariableWithSize (dimension d) 
  --seEnabled <- unNamedVariableWithSize (dimension d) 

  cpt se [dv d] ~~ [1.0,0.0,1.0,0.0]
  --cpt seEnabled [dv se] ~~ [1.0,0.0,0.0,1.0] -- No info about the observation of the soft evidence node
  return (tdv se) 

-- | Soft evidence factor
se :: Factor f 
   => TDV s -- ^ Soft evidence node
   -> TDV s -- ^ Node on which the soft evidence is imposed
   -> Double -- ^ Soft evidence (probability of right detection)
   -> Maybe f
se s orgNode p = factorWithVariables [dv s,dv orgNode] [p,1-p,1-p,p]

{-

Helper functions to create logical distributions  

-}

data LE = LETest DVI
        | LEAnd LE LE 
        | LEOr LE LE 
        | LENot LE 
        deriving(Eq)

-- | Generate the variables used in the expression
varsFromLE :: LE -> [DV]
varsFromLE le = nub $ _getVars le 
 where 
  _getVars  (LETest dvi) = [dv dvi] 
  _getVars (LEAnd a b) = _getVars a ++ _getVars b
  _getVars (LEOr a b) = _getVars a ++ _getVars b
  _getVars (LENot a) = _getVars a

boolValue :: Maybe Bool -> Bool 
boolValue (Just True) = True 
boolValue _ = False

-- | Generate values for the LE
functionFromLE :: LE -> ([DVI] -> Bool)
functionFromLE (LETest dvi) = \i -> boolValue $ do 
  var <- L.find (== dvi) i
  return (instantiationValue dvi == instantiationValue var)
functionFromLE (LENot l) = \i -> not (functionFromLE l i)
functionFromLE (LEAnd la lb) = \i -> (functionFromLE la i) && (functionFromLE lb i)
functionFromLE (LEOr la lb) = \i -> (functionFromLE la i) || (functionFromLE lb i)

class Testable d v where 
  -- | Create a variable instantiation using values from
  -- an enumeration
  (.==.) :: d -> v -> LE 

instance Instantiable d v DVI => Testable d v where 
  (.==.) a b = LETest (a =: b)

infixl 8 .==.
infixl 6 .&.
infixl 5 .|.

(.|.) :: LE -> LE -> LE
(.|.)  = LEOr 

(.&.) :: LE -> LE -> LE
(.&.) = LEAnd

(.!.) :: LE -> LE
(.!.) = LENot

logical :: (Factor f, DirectedGraph g) => TDV Bool -> LE -> BNMonad g f () 
logical dv l = 
  let theVars = varsFromLE l
      logicalF = functionFromLE l 
      probaVal True = 1.0 :: Double
      probaVal False = 0.0 :: Double
      valuesF = [probaVal (logicalF i == False) | i <-forAllInstantiations (DVSet theVars)]
      valuesT = [probaVal (logicalF i == True) | i <-forAllInstantiations (DVSet theVars)]

  in 
  cpt dv theVars ~~ (valuesF ++ valuesT)

{-

Noisy OR

-}

-- | Noisy AND. Variable A is passed with probability 1-p
noisyAND :: (DirectedGraph g, Factor f, NamedGraph g) => TDV Bool -> Double -> BNMonad g f (TDV Bool) 
noisyAND a p = do 
    na <- unamedVariable (t::Bool)
    cpt na [dv a] ~~ [1-p,p,p,1-p]
    return na 

-- | OR Gate
orG :: (DirectedGraph g, Factor f, NamedGraph g) => TDV Bool -> TDV Bool -> BNMonad g f (TDV Bool)
orG a b = do 
    no <- unamedVariable (t::Bool)
    logical no ((a .==. True) .|. (b .==. True))
    return no 

-- | Noisy OR. The Noisy-OR with leak can be implemented by using the
-- standard Noisy-OR and a leak variable.
noisyOR :: (DirectedGraph g, Factor f, NamedGraph g) 
        => [(TDV Bool,Double)] -- ^ Variables and probability of no influence
        -> BNMonad g f (TDV Bool) 
noisyOR l = do 
    a <- mapM (\(a,p) -> noisyAND a p) l
    foldM orG (head a) (tail a)

{-
 
Graph creation from the Monad.  

-}

-- | Create a  network using the simple graph implementation
-- The initialized nodes are replaced by the value.
-- Returns the monad values and the built graph.
runBN :: BNMonad DirectedSG f a -> (a,SBN f)
runBN = runNetwork

-- | Create a  network but only returns the monad value.
-- Mainly used for testing.
execBN :: BNMonad DirectedSG f a -> SBN f
execBN = execNetwork

-- | Create a bayesian network but only returns the monad value.
-- Mainly used for testing.
evalBN :: BNMonad DirectedSG f a -> a
evalBN = evalNetwork


