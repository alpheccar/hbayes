{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | Tools to build influence diagrams

-}
module Bayes.InfluenceDiagram(
  -- * Type
    InfluenceDiagram
  , DecisionFactor
  , Instantiable(..)
  , DEV 
  , UV
  , DV
  , TDV
  , IDMonad
  -- * Building 
  , t 
  , (~~)
  , chance 
  , decisionNode 
  , utilityNode 
  , proba 
  , decision 
  , utility
  , cpt
  , d 
  , p
  , noDependencies
  -- * Solving
  , decisionsOrder
  , solveInfluenceDiagram
  , runID
  , policyNetwork
  , decisionToInstantiation
  -- * Testing 
  , DVISet 
  , DVI
	) where 

import Bayes
import Bayes.PrivateTypes
import Bayes.Network 
import Data.Monoid
import Bayes.Factor
import Bayes.Factor.PrivateCPT
import Bayes.Factor.CPT
import Bayes.Factor.MaxCPT
import Data.Maybe(fromJust,mapMaybe)
import Control.Applicative((<$>))
import Bayes.VariableElimination.Buckets
import Bayes.Factor.PrivateCPT(DecisionFactor(..),decisionFactor,convertToMaxFactor,convertToNormalFactor,privateFactorValue,_factorVariables)
import Data.List(foldl1',foldl')
import Control.Monad.State.Strict(gets)
import qualified Data.Vector as NV
import qualified Data.Map as M 
import qualified Data.IntMap as IM 

--import Debug.Trace 

--debug s a = trace (s ++ show a) a

replaceDecisionNodeWithPolicy :: InfluenceDiagram -> CPT -> InfluenceDiagram 
replaceDecisionNodeWithPolicy g f = 
  let dv = factorMainVariable f 
      parentVariables = tail (factorVariables f)
      factorV = vertex dv
      g' = fromJust $ changeVertexValue factorV (DecisionNode f) g
      oldParentEdges = fromJust $ ingoing g' factorV 
      g'' = foldr removeEdge g' oldParentEdges
      addNewFactorEdge pdv currentG = addEdge (edge (vertex pdv) (vertex dv)) NormalLink currentG
  in 
  -- When we add the edges, we don't care about the order because
  -- the CPT is already created and we don't need the order of the parents in the graph
  -- to deduce the order of the variables in the CPT.
  foldr addNewFactorEdge g'' parentVariables

-- | Create a policy factor from a decision factor
policyFactor :: DecisionFactor -> CPT
policyFactor (Scalar v) = 
  let decisionVar = v 
      originalV = vertex decisionVar 
      nakedVars = [dv decisionVar]
      allVars = DVSet nakedVars
      values = do 
        x <- forAllInstantiations allVars 
        if (instantiationValue v == instantiationValue (head x))
          then 
            return 1.0 
          else 
            return 0.0
  in 
  fromJust . factorWithVariables nakedVars $ values 
policyFactor f@(Table d m v) = 
  let decisionVar = NV.head v 
      originalV = vertex decisionVar 
      nakedVars = dv decisionVar : d
      allVars = DVSet nakedVars
      values = do 
        x <- forAllInstantiations allVars 
        let v = privateFactorValue f (tail x)
        if (instantiationValue v == instantiationValue (head x))
          then 
            return 1.0 
          else 
            return 0.0
  in
  fromJust . factorWithVariables nakedVars $ values
  

-- | Convert a decision policy to a set of possible instantiations
-- It is the only way to access to the content of a decision factor.
decisionToInstantiation :: DecisionFactor -> [DVISet]
decisionToInstantiation f@(Scalar v) = [[v]]
decisionToInstantiation f@(Table d m v) = 
  let allVars = DVSet d
      values = do 
        x <- forAllInstantiations allVars 
        let v = privateFactorValue f x 
        return (v:x)
  in 
  values

-- | Create a policy network from an influence diagram and its solution.
-- A policy network is a Bayesian network where the decision nodes have been replaced
-- with probability nodes where the probability is 1 when the configuration is corresponding
-- to the decision and 0 otherwise.
policyNetwork :: [DecisionFactor] -> InfluenceDiagram -> SBN CPT 
policyNetwork l idg = 
  let idg1 = foldl' replaceDecisionNodeWithPolicy idg (map policyFactor l) 
      utilities = filter (isUtilityNode idg) . allVertices $ idg
      SP e v b = foldr removeVertex idg1 utilities
      toBayesNode (l,ChanceNode f) = (l,f) 
      toBayesNode (l,DecisionNode f) = (l,f)
      toBayesNode (l,UtilityNode _ _) = error "No utilities nodes should remain to create the policy network"
      e' = M.map (const ()) e 
      v' = IM.map toBayesNode v 
  in 
  SP e' v' b


instance Show DecisionFactor where
    show (Scalar v) = "\nScalar Factor:\n" ++ show v
    show c@(Table [] _ v) = "\nEmpty DecisionFactor:\n"

    show c = displayFactorBody c 

instance MultiDimTable DecisionFactor where 
    elementStringValue f d = show (privateFactorValue f d)
    tableVariables = _factorVariables


data JoinSum = JS !CPT !CPT deriving(Eq)

instance Show JoinSum where 
  show (JS p u) = "CPT\n" ++ show p ++ "\nUTILITY\n" ++ show u ++ "\n"

chanceFactor f = JS f (factorFromScalar 0.0)
utilityFactor f = JS (factorFromScalar 1.0) f

jsProduct ::  JoinSum -> JoinSum -> JoinSum
jsProduct (JS pa ua) (JS pb ub) = JS (itemProduct [pa,pb]) (cptSum [ua,ub])


-- | Max out a variable
maximalize :: DV -> [JoinSum] -> (JoinSum,DecisionFactor) 
maximalize dv l = 
  let JS pa ua = itemProduct l
      maxa = convertToNormalFactor . itemProjectOut dv . convertToMaxFactor $ pa
      maxu' = itemProjectOut dv . convertToMaxFactor  . itemProduct $ [pa,ua]
      maxu = convertToNormalFactor maxu'
      instF = decisionFactor maxu'
  in 
  (JS maxa (cptDivide maxu maxa),instF)



instance IsBucketItem JoinSum where
    scalarItem (JS a b) = isScalarFactor a && isScalarFactor b
    itemProduct l = foldl1' jsProduct l
    itemProjectOut dv (JS pa ua) =  
      let suma = itemProjectOut dv pa
          sumu = itemProjectOut dv (itemProduct [pa,ua])
      in 
      JS suma (cptDivide sumu suma)
    itemContainsVariable (JS a b) dv = containsVariable a dv || containsVariable b dv


-- | Synonym for undefined because it is clearer to use t to set the Enum bounds of a variable
t = undefined

-- | Edge kind
data EdgeKind = NormalLink 
              deriving(Eq,Show)

isInformationLink :: InfluenceDiagram -> Edge -> Bool
isInformationLink g (Edge va vb) = 
  (isChanceNode g va || isDecisionNode g va) && (isDecisionNode g vb)

isRevealedChanceNode :: InfluenceDiagram -> Vertex -> Bool 
isRevealedChanceNode g v = isChanceNode g v && any (isDecisionNode g) (childrenNodes g v)

edgeShape :: InfluenceDiagram -> Edge -> EdgeKind -> Maybe String
edgeShape g e NormalLink | isInformationLink g e = Just "style=dashed"
                         | otherwise = Nothing 
edgeColor :: InfluenceDiagram -> Edge -> EdgeKind -> Maybe String
edgeColor _ _ _ = Nothing

nodeShape :: InfluenceDiagram -> Vertex -> IDValue -> Maybe String
nodeShape _ _ (ChanceNode _) = Just "shape=ellipse" 
nodeShape _ _ (UtilityNode _ _) = Just "shape=diamond"
nodeShape _ _ (DecisionNode _) = Just "shape=box"

nodeColor :: InfluenceDiagram -> Vertex -> IDValue -> Maybe String
nodeColor g v (ChanceNode _) | isRevealedChanceNode g v = Just "style=filled,fillcolor=gray"
                             | otherwise = Nothing
nodeColor _ _ _ = Nothing

instance Show InfluenceDiagram where
  show g = displaySimpleGraph (nodeShape g) (nodeColor g) (edgeShape g) (edgeColor g) g


instance Monoid EdgeKind where 
   mempty = NormalLink 
   NormalLink `mappend` NormalLink = NormalLink

-- | Influence diagram
type InfluenceDiagram = DirectedSG EdgeKind IDValue

type IDMonad g a = NetworkMonad g EdgeKind IDValue a


-- Most factors are coding for f(abc) where a is the main factor variable
-- and where the a vertex is the original vertex in the graph.
-- For an utility, we have U(abcd) where the variables a is NOT the main factor variable.
-- Indeed, the main factor variable if it was used in the factor would have dimension 1.
-- So, it is useless in the factor.
-- So, we need another DV field to track the original vertex
data IDValue   = ChanceNode !CPT
               | UtilityNode !DV !CPT 
               | DecisionNode !CPT
               deriving(Eq)

dvFromIDValue (ChanceNode f) = factorMainVariable f
dvFromIDValue (UtilityNode dv f) = dv
dvFromIDValue (DecisionNode f) = factorMainVariable f

factorVariablesFromIDValue (ChanceNode f) = factorVariables f
factorVariablesFromIDValue (UtilityNode _ f) = factorVariables f
factorVariablesFromIDValue (DecisionNode f) = factorVariables f

jsFromIDValue (ChanceNode f) = chanceFactor f
jsFromIDValue (UtilityNode _ f) = utilityFactor f
jsFromIDValue (DecisionNode _) = error "You don't need to get the factor for a decision node"

instance Show IDValue where 
   show (ChanceNode f) = "CHANCE:\n" ++ show f
   show (UtilityNode _ f) = "UTILITY:\n" ++ show f
   show (DecisionNode f) = ""


-- | Utility variable
data UV = UV !Vertex !Int deriving(Eq)

-- | Decision variable
data DEV = DEV !Vertex !Int deriving(Eq,Ord)

instance Show DEV where
    show (DEV v d) = "D" ++ show v ++ "(" ++ show d ++ ")"

instance BayesianVariable DEV where 
  vertex (DEV v _) = v

instance BayesianDiscreteVariable DEV where 
  dimension (DEV _ d) = d 
  dv (DEV v d) = DV v d 

instance Instantiable DEV Int DVI where 
  (=:) d@(DEV v dim) value = DVI (dv d) value

data PorD = P DV | D DEV deriving(Eq)

class ChanceVariable m where 
  toDV :: m -> DV

instance ChanceVariable DV where 
  toDV = dv 

instance ChanceVariable (TDV s) where 
  toDV = dv

instance BayesianVariable PorD where 
    vertex (D d) = vertex d
    vertex (P p) = vertex p

instance BayesianDiscreteVariable PorD where
    dimension (D d) = dimension d
    dimension (P p) = dimension p
    dv (D x) = dv x
    dv (P x) = dv x


-- | Used to mix decision and chance variables and a same list
p :: ChanceVariable c => c -> PorD
p = P . toDV

-- | Used to mix decision and chance variables and a same list
d :: DEV -> PorD 
d = D



-- | Create a chance node
chance :: (Bounded a, Enum a, NamedGraph g)
       => String 
       -> a 
       -> IDMonad g (TDV a)
chance = variable

-- | Create an utility node
utilityNode :: (NamedGraph g)
            => String 
            -> IDMonad g UV
utilityNode s = do
  DV v i <- variableWithSize s 1
  return (UV v i)

-- | Create a decision node
decisionNode :: (Bounded a, Enum a, NamedGraph g)
             => String 
             -> a
             -> IDMonad g DEV
decisionNode s a =  do
  DV v i <- variable s a >>= return . dv
  return (DEV v i)

utilityCpt :: (DirectedGraph g, Distribution d, Factor f) 
           => Vertex -- ^ Vertex containing the factor
           -> d -- ^ Distribution to initialize the factor
           -> NetworkMonad g e a (Maybe f) 
utilityCpt v l  = do 
  g <- gets snd
  let vertices = map (fromJust . startVertex g) . fromJust . ingoing g $ v
  fv <- mapM factorVariable vertices
  let cpt = createFactor (map fromJust fv) l
  return cpt

class Initializable v where 
  (~~) :: (DirectedGraph g, Distribution d) 
     => IDMonad g v -- ^ Discrete variable in the graph
     -> d -- ^ List of values
     -> IDMonad g ()

instance Initializable DV where
  (~~) mv l = do 
     (DV v _) <- mv >>= return . dv -- This is updating the state and so the graph
     maybeNewValue <- getCpt v l
     currentValue <- getBayesianNode v
     case (currentValue, maybeNewValue) of 
       (Just c, Just n) -> initializeNodeWithValue v c (ChanceNode n)
       _ -> return ()

instance Initializable (TDV s) where
  (~~) mv l = do 
     (DV v _) <- mv >>= return . dv -- This is updating the state and so the graph
     maybeNewValue <- getCpt v l
     currentValue <- getBayesianNode v
     case (currentValue, maybeNewValue) of 
       (Just c, Just n) -> initializeNodeWithValue v c (ChanceNode n)
       _ -> return ()

instance Initializable UV where
  (~~) mv l = do 
     (UV v dim) <- mv  -- This is updating the state and so the graph
     maybeNewValue <- utilityCpt v l
     currentValue <- getBayesianNode v
     case (currentValue, maybeNewValue) of 
       (Just c, Just n) -> initializeNodeWithValue v c (UtilityNode (DV v dim) n)
       _ -> return ()

instance Initializable DEV where
  (~~) mv l = do 
     (DV v _) <- mv >>= return . dv -- This is updating the state and so the graph
     maybeNewValue <- getCpt v l
     currentValue <- getBayesianNode v
     case (currentValue, maybeNewValue) of 
       (Just c, Just n) -> initializeNodeWithValue v c (DecisionNode n)
       _ -> return ()

_cpt :: (DirectedGraph g , BayesianDiscreteVariable v,BayesianDiscreteVariable vb) => v -> [vb] -> IDMonad g v
_cpt node conditions = do
  mapM_ ((dv node) <--) (reverse (map dv conditions))
  return node

-- | Define that a chance node is a conditional probability and define the parent variables
cpt :: (DirectedGraph g ,BayesianDiscreteVariable vb, ChanceVariable c) => c -> [vb] -> IDMonad g c
cpt node conditions = do
  mapM_ ((toDV node) <--) (reverse (map dv conditions))
  return node

-- | Define that a chance node is a probability (not conditional)
-- Values are ordered like
-- FFF FFT FTF FTT TFF TFT TTF TTT
-- and same for other enumeration keeping enumeration order
proba :: (ChanceVariable c, DirectedGraph g) => c -> IDMonad g c
proba node = cpt node ([] :: [DV])

-- | Define a utility dependence
utility :: (DirectedGraph g , BayesianDiscreteVariable dv) => UV -> [dv] -> IDMonad g UV
utility (UV v d) l = do 
  DV v' d' <- _cpt (DV v d) l
  return (UV v' d')

-- | Used to define a root decision which is not dependent on any past node
noDependencies :: [DV]
noDependencies = []

-- | Define a decision dependence
decision :: (DirectedGraph g, BayesianDiscreteVariable dv) => DEV -> [dv] -> IDMonad g DEV
decision d l = do 
  let dim = product . map dimension $ dv d:map dv l
  _cpt d l ~~ (replicate dim 1.0)
  return d


-- | Run an influence monad
runID :: IDMonad DirectedSG a -> (a,InfluenceDiagram)
runID = runNetwork 

{-
 
Generation of temporal order

-}

maybeOnlyResult :: [a] -> Maybe a 
maybeOnlyResult [a] = Just a 
maybeOnlyResult _ = Nothing

isDecisionNode :: InfluenceDiagram -> Vertex -> Bool 
{-# INLINE isDecisionNode #-}
isDecisionNode g v = maybe False (const True) $ do
  DecisionNode f <- vertexValue g v
  return f

isUtilityNode :: InfluenceDiagram -> Vertex -> Bool 
{-# INLINE isUtilityNode #-}
isUtilityNode g v = maybe False (const True) $ do
  UtilityNode _ f <- vertexValue g v
  return f

isChanceNode :: InfluenceDiagram -> Vertex -> Bool 
{-# INLINE isChanceNode #-}
isChanceNode g v = maybe False (const True) $ do
  ChanceNode f <- vertexValue g v
  return f

-- | Check the node is a decision node and none of its parents are decision nodes
isRootDecision :: InfluenceDiagram -> Vertex -> Bool
{-# INLINE isRootDecision #-}
isRootDecision g v | isDecisionNode g v = 
  case ingoing g v of 
    Just [] -> True 
    _ -> False
                   | otherwise = False

-- | Get the chance parents of a decision node and a new graph with those parents
-- removed and the decision node removed
chanceParents :: DEV -> InfluenceDiagram -> (InfluenceDiagram,[DV])
chanceParents dev currentG = 
  let p = filter (isChanceNode currentG) . parentNodes currentG $ (vertex dev) 
      theParents = map (vertexToDV currentG) p
      newG = foldr removeVertex currentG (vertex dev : p)
  in 
  (newG,theParents)

-- | Return the remaining chance nodes of the graph
remainingChanceNodes :: InfluenceDiagram -> [DV]
remainingChanceNodes = chanceNodes 

-- | Return the utility nodes
utilityNodes :: InfluenceDiagram -> [UV]
utilityNodes g = map (vertexToUV g) . filter (isUtilityNode g) . allVertices $ g

-- | Return the chance nodes
chanceNodes :: InfluenceDiagram -> [DV]
chanceNodes g = map (vertexToDV g) . filter (isChanceNode g) . allVertices $ g

-- | Return all chances factor and utility factors
chanceAndUtilityFactors :: InfluenceDiagram -> [JoinSum]
chanceAndUtilityFactors g = map (jsFromIDValue . fromJust . vertexValue g) . filter (not . isDecisionNode g) . allVertices $ g

vertexToDV :: InfluenceDiagram -> Vertex -> DV 
vertexToDV g v = dvFromIDValue . fromJust . vertexValue g $ v

vertexToDEV :: InfluenceDiagram -> Vertex -> DEV 
vertexToDEV g v = 
  let DV v1 d = vertexToDV g v 
  in 
  DEV v1 d

vertexToUV :: InfluenceDiagram -> Vertex -> UV 
vertexToUV g v = 
  let DV v1 d = vertexToDV g v 
  in 
  UV v1 d

-- | Return a root decision
rootDecision :: InfluenceDiagram -> Maybe Vertex 
rootDecision g = do 
    r <- rootNode g 
    if isDecisionNode g r 
      then
        return r 
      else 
        rootDecision (removeVertex r g)

-- | Used to encode the temporal order
data ChancesOrDecision = C ![DV] | DEC !DEV deriving(Eq,Ord,Show)

dvOrder :: [ChancesOrDecision] -> [DV] 
dvOrder [] = []
dvOrder (C l:r) = l ++ dvOrder r 
dvOrder (DEC d:r) = dv d: dvOrder r 

-- | Remove all decisions node and record their chance parents
removeAndRecordRootDecision :: [ChancesOrDecision] -> InfluenceDiagram -> [ChancesOrDecision]
removeAndRecordRootDecision currentL currentG = 
  case vertexToDEV currentG <$> (rootDecision currentG) of 
    Nothing -> (C (remainingChanceNodes currentG)):currentL 
    Just newD -> 
      let (currentG', p) = chanceParents newD currentG
      in
      removeAndRecordRootDecision ((DEC newD):(C p):currentL) currentG'

-- | List of decision vertices in reverse temporal order (corresponding to elimination order)
decisionsOrder :: InfluenceDiagram -> [ChancesOrDecision] 
decisionsOrder g = removeAndRecordRootDecision [] $ g 

-- | Maximalize the decision variable to find the decision strategy at the current set
maximalizeOneVariable :: Buckets JoinSum -> DV -> (Buckets JoinSum,DecisionFactor)
maximalizeOneVariable currentBucket dv   = 
  let fk = getBucket dv currentBucket
      (newF, instF) = maximalize dv fk
  in
  (updateBucket dv newF currentBucket, instF)

--debugs True f = \a b -> debug ("SUM OUT " ++ show b ++ "\n") (f a b)
--debugs False f = \a b -> debug ("MAX OUT " ++ show b ++ "\n") (f a b)

marginalizeID :: [ChancesOrDecision]  -> Buckets JoinSum -> [DecisionFactor] -> (Buckets JoinSum,[DecisionFactor])
marginalizeID [] b r = (b,r)
marginalizeID (C d:r) currentB currentR =  
  let bucket' = foldl' marginalizeOneVariable currentB d 
  in 
  marginalizeID r bucket' currentR 
marginalizeID (DEC de:r) currentB currentR = 
  let (bucket',instF) = maximalizeOneVariable currentB (dv de) 
  in 
  marginalizeID r bucket' (instF:currentR) 

-- | Solve an influence diagram. A DecisionFactor is generated for each decision variable.
-- A decision factor is containing a variable instantiation instead of a double.
-- This instantiation is giving the decision to take for each value of the parents.
solveInfluenceDiagram :: InfluenceDiagram -> [DecisionFactor]
solveInfluenceDiagram g = 
  let decOrder = decisionsOrder g
      theFactors = chanceAndUtilityFactors g
      p = dvOrder decOrder 
      bucket = createBuckets theFactors p []
      (_, result) = marginalizeID decOrder bucket []
  in
  result 

