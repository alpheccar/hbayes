{-# LANGUAGE ViewPatterns #-}
{- | Common functions and types for building networks

-}
module Bayes.Network(
  -- * Types 
    MaybeNode(..)
  , NetworkMonad(..)
  -- * Functions
  , factorVariable
  , (<--)
  , getBayesianNode 
  , setBayesianNode
  , initializeNodeWithValue
  , setVariableBoundWithSize
  , setVariableBound
  , addVariableIfNotFound
  , unamedVariable
  , variable
  , variableWithSize
  , unNamedVariableWithSize
  , runNetwork
  , execNetwork
  , evalNetwork
  , runGraph
  , execGraph
  , evalGraph
  , getCpt
  ) where 

import Bayes.PrivateTypes
import Bayes 
import Control.Monad.State.Strict
import Bayes.Tools 
import Data.Maybe(fromJust)
import Bayes.Factor
import Data.Monoid

-- | Bayesian variable : name,dimension, factor
-- When initialized it is using a factor with bayesian variables.
-- But the factor value are not yet set
data MaybeNode f = UninitializedNode String Int
                 | InitializedNode String Int f


-- | The Network monad
type NetworkMonad g e f a = GraphMonad g e (MaybeNode f) a


-- | Get the Bayesian Discrete Variable for a vertex.
-- It works because we keep the variable dimension during creating of the graph
factorVariable :: Graph g => Vertex -> NetworkMonad g e f (Maybe DV)  
factorVariable v = do 
  g <- gets snd 
  let value = vertexValue g v
  case value of
    Nothing -> return Nothing
    Just (UninitializedNode _ d) -> return $ Just $ DV v d
    Just (InitializedNode _ d _) -> return $ Just $ DV v d


-- | Create an edge between two vertex of the Bayesian network
(<--) :: (Graph g, BayesianDiscreteVariable dv, Monoid e) => dv -> dv -> NetworkMonad g e f ()
(dv -> DV va _) <-- (dv -> DV vb _) = newEdge vb va mempty

whenJust Nothing _ = return ()
whenJust (Just i) f = f i >> return ()

getCpt :: (DirectedGraph g, Distribution d, Factor f) 
       => Vertex -- ^ Vertex containing the factor
       -> d -- ^ Distribution to initialize the factor
       -> NetworkMonad g e a (Maybe f) 
getCpt v l  = do 
  g <- gets snd
  currentVar <- factorVariable v
  let vertices = map (fromJust . startVertex g) . fromJust . ingoing g $ v
  fv <- mapM factorVariable vertices
  let cpt = createFactor (map fromJust (currentVar:fv)) l
  return cpt

-- | Get the node of a bayesian network under creation
getBayesianNode :: Graph g => Vertex -> NetworkMonad g e f (Maybe (MaybeNode f))
getBayesianNode v = do
  g <- gets snd
  return $ vertexValue g v

-- | Set the node of a bayesian network under creation
setBayesianNode :: Graph g => Vertex -> MaybeNode f -> NetworkMonad g e f ()
setBayesianNode v newValue = do
  (aux,oldGraph) <- get
  let newGraph = changeVertexValue v newValue oldGraph
 
  whenJust newGraph $ \nvm -> do
     put $! (aux, nvm)

-- | Set the value of uninitialized nodes. Initialized nodes are not changed.
initializeNodeWithValue :: Graph g 
                        => Vertex -- ^ Vertex
                        -> MaybeNode a -- ^ Current uninitialized node
                        -> a -- ^ Value to set
                        -> NetworkMonad g e a () 
initializeNodeWithValue _ (InitializedNode _ _ _) _ = return ()
initializeNodeWithValue v (UninitializedNode s dim) newValue = do 
  g <- gets snd
  setBayesianNode v (InitializedNode s dim newValue)

-- | Set the bound of a bayesian variable (number of levels)
setVariableBoundWithSize :: Graph g
                         => Vertex -- ^ Vertex
                         -> Int -- ^ Inf limit  (0 for instance)
                         -> Int -- ^ Sup limit (1 for instance for 2 elements)
                         -> NetworkMonad g e f ()
setVariableBoundWithSize a bmin bmax = do
    v <- getBayesianNode a
    whenJust v $ \(UninitializedNode s _) -> do
      setBayesianNode a (UninitializedNode s (bmax - bmin + 1))

setVariableBound :: (Enum a, Bounded a, Graph g) 
                 => Vertex -- ^ Vertex
                 -> a -- ^ Bounded variable (t :: type where t is undefined)
                 -> NetworkMonad g e f ()
setVariableBound a e = 
  let bmin = intValue $ minBoundForEnum e
      bmax = intValue $ maxBoundForEnum e
  in 
  setVariableBoundWithSize a bmin bmax

-- | Create a new named Bayesian variable if not found.
-- Otherwise, return the found one.
addVariableIfNotFound :: NamedGraph g => String -> NetworkMonad g e f Vertex
addVariableIfNotFound vertexName = graphNode vertexName (UninitializedNode vertexName 0)

-- | Initialize a new variable
_initializeVariableBounds :: (Enum a, Bounded a, NamedGraph g)
                          => Vertex 
                          -> a 
                          -> NetworkMonad g e f (TDV a)
_initializeVariableBounds va e = do 
  setVariableBound va e
  maybeValue <- getBayesianNode va 
  case fromJust maybeValue of 
     UninitializedNode s d -> return (tdv $ DV va d)
     InitializedNode _ d _ -> return (tdv $ DV va d) 

-- | Initialize a new variable with size
_initializeVariableBoundsWithSize :: NamedGraph g
                                  => Vertex -- ^ Variable name
                                  -> Int -- ^ Variable size
                                  -> NetworkMonad g e f DV
_initializeVariableBoundsWithSize va e = do
  setVariableBoundWithSize va 0 (e-1)
  maybeValue <- getBayesianNode va 
  setBayesianNode va (fromJust maybeValue)
  case fromJust maybeValue of 
     UninitializedNode s d -> return (DV va d)
     InitializedNode _ d _ -> return (DV va d)

-- | Create a new unamed variable
unamedVariable :: (Enum a, Bounded a, NamedGraph g)
               => a -- ^ Variable bounds 
               -> NetworkMonad g e f (TDV a)
unamedVariable e = do 
  va <- getNewEmptyVariable Nothing (UninitializedNode "unamed" 0)
  _initializeVariableBounds va e

-- | Define a Bayesian variable (name and bounds)
variable :: (Enum a, Bounded a, NamedGraph g) 
        => String -- ^ Variable name
        -> a -- ^ Variable bounds
        -> NetworkMonad g e f (TDV a)
variable name e = do
  va <- addVariableIfNotFound name
  _initializeVariableBounds va e

-- | Define a Bayesian variable (name and bounds)
variableWithSize :: NamedGraph g
        => String -- ^ Variable name
        -> Int -- ^ Variable size
        -> NetworkMonad g e f DV
variableWithSize name e = do
  va <- addVariableIfNotFound name
  _initializeVariableBoundsWithSize va e

-- | Define a Bayesian variable (name and bounds)
unNamedVariableWithSize :: NamedGraph g
                        => Int -- ^ Variable size
                        -> NetworkMonad g e f DV
unNamedVariableWithSize e = do
  va <- getNewEmptyVariable Nothing (UninitializedNode "unamed" 0)
  _initializeVariableBoundsWithSize va e

-- | Create a  network using the simple graph implementation
-- The initialized nodes are replaced by the value.
-- Returns the monad values and the built graph.
runNetwork :: NetworkMonad DirectedSG e f a -> (a,DirectedSG e f)
runNetwork x = 
  let (r,g) = runGraph x
      convertNodes (InitializedNode s d f) = f 
      convertNodes (UninitializedNode s d) = error $ "All variables must be initialized with a factor: " ++ s ++ "(" ++ show d ++ ")"
  in 
  (r,fmap convertNodes g)

-- | Create a  network but only returns the monad value.
-- Mainly used for testing.
execNetwork :: NetworkMonad DirectedSG e f a -> DirectedSG e f
execNetwork x = 
  let g = execGraph x
      convertNodes (InitializedNode s d f) = f 
      convertNodes (UninitializedNode s d) = error $ "All variables must be initialized with a factor: " ++ s ++ "(" ++ show d ++ ")"
  in 
  fmap convertNodes g


-- | Create a bayesian network but only returns the monad value.
-- Mainly used for testing.
evalNetwork :: Graph g => NetworkMonad g e f a -> a
evalNetwork = evalGraph
