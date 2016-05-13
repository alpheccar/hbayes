{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | Bayesian Network Library.

It is a very preliminary version. It has only been tested on very simple
examples where it worked. It should be considered as experimental and not used
in any production work.

* Look at the "Bayes.Examples" and "Bayes.Examples.Tutorial" in this package 
to see how to use the library.

* In "Bayes.Examples.Influence" you'll find additional examples about influence diagrams.

* In "Bayes.Examples.Sampling" there are some explanations about the samplers for discrete networks.

* "Bayes.Examples.EMTest" is explaining learning with expectation / maximization.

* "Bayes.Examples.ContinuousSampling" is showing an example of sampling with a continuous network.

-}
module Bayes(
  -- * Graph
  -- ** Graph classes
    Graph(..)
  , UndirectedGraph(..)
  , DirectedGraph(..)
  , FoldableWithVertex(..)
  , FunctorWithVertex(..)
  , NamedGraph(..)
  -- ** Graph Monad
  , GraphMonad
  , GMState(..)
  , graphNode
  , runGraph
  , execGraph
  , evalGraph
  , emptyAuxiliaryState
  , getNewEmptyVariable
  , isRoot
  , rootNode
  , parentNodes
  , childrenNodes
  , markovBlanket
  -- ** Support functions for Graph constructions
  , Vertex
  , Edge 
  , edge
  , newEdge
  , getVertex
  , edgeEndPoints
  , connectedGraph
  , dag
  , printGraphValues
  -- * SimpleGraph implementation
  -- ** The SimpleGraph type
  , DirectedSG
  , UndirectedSG
  , SBN(..)
  , varMap
  , displaySimpleGraph
  -- ** Bayesian network
  , BayesianNetwork(..)
  -- * Testing
  , testEdgeRemoval_prop
  , testVertexRemoval_prop
) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Applicative((<$>))
import Bayes.Factor hiding(isEmpty)
import Bayes.Factor.CPT(CPT(..))
import Bayes.Factor.MaxCPT(MAXCPT(..))
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Traversable as T 
import Control.Applicative 
import qualified Data.Set as Set

import Test.QuickCheck hiding ((.&.),Testable)
import Test.QuickCheck.Arbitrary
import Data.List(sort,intercalate,nub,foldl')
import Bayes.PrivateTypes hiding(isEmpty)
import GHC.Float(float2Double)

--import Debug.Trace
--debug a = trace (show a) a

-- | An implementation of the BayesianNetwork using the simple graph and no value for the edges
type SBN f = DirectedSG () f



instance Arbitrary (DirectedSG String String) where
  arbitrary = do 
    let createVertex g i = do 
          name <- arbitrary :: Gen String
          return $ addVertex (Vertex i) name g
        createEdge g (va,vb) = do 
          name <- arbitrary :: Gen String
          return $ addEdge (edge va vb) name g 

    nbVertex <- choose (1,8) :: Gen Int
    
    g <- foldM createVertex emptyGraph [1..nbVertex]

    let allPairs = [(Vertex x,Vertex y) | x <- [1..nbVertex], y <- [1..nbVertex], x /= y]
        anEdge (x,y) = arbitrary :: Gen Bool

    edges <- filterM anEdge allPairs

    foldM createEdge g edges

instance Arbitrary (DirectedSG () String) where
  arbitrary = do 
    let createVertex g i = do 
          name <- arbitrary :: Gen String
          return $ addVertex (Vertex i) name g
        createEdge g (va,vb) = do 
          return $ addEdge (edge va vb) () g 

    nbVertex <- choose (1,8) :: Gen Int
    
    g <- foldM createVertex emptyGraph [1..nbVertex]

    let allPairs = [(Vertex x,Vertex y) | x <- [1..nbVertex], y <- [1..nbVertex], x /= y]
        anEdge (x,y) = arbitrary :: Gen Bool

    edges <- filterM anEdge allPairs

    foldM createEdge g edges   

-- | Warning : the generated graph is not at all a bayesian network
-- The variables in the CPT have no reason to correspond to the edges
-- connected to that CPT.
-- Only the main variable (first variable) is linked to the right vertex
instance Factor f => Arbitrary (DirectedSG () f) where
  arbitrary = do 
    let createVertex g i = do 
          let value = fromJust $ factorWithVariables [DV (Vertex i) 2] [0.1,0.9]
          return $ addVertex (Vertex i) value g
        createEdge g (va,vb) = do 
          return $ addEdge (edge va vb) () g 

    nbVertex <- choose (1,8) :: Gen Int
    
    g <- foldM createVertex emptyGraph [1..nbVertex]

    let allPairs = [(Vertex x,Vertex y) | x <- [1..nbVertex], y <- [1..nbVertex], x /= y]
        anEdge (x,y) = arbitrary :: Gen Bool

    edges <- filterM anEdge allPairs

    foldM createEdge g edges

testEdgeRemoval_prop :: DirectedSG String String -> Property
testEdgeRemoval_prop g = (not . hasNoEdges) g ==> 
  let Just e = someEdge g
      Just (vs,ve) = edgeVertices g e
      Just bi = ingoing g ve
      Just bo = outgoing g vs
      g' = removeEdge e g 
      Just bi' = ingoing g' ve
      Just bo' = outgoing g' vs
  in
  (map (sort . (:) e ) [bi', bo'] == map sort [bi,bo]) &&
   (sort (allEdges g) == sort (e:allEdges g'))

testVertexRemoval_prop :: DirectedSG String String -> Property
testVertexRemoval_prop g = (not . hasNoVertices) g ==>
    let Just v = someVertex g
        Just bi = ingoing g v 
        Just bo = outgoing g v
        g' = removeVertex v g
        srcVertices = mapMaybe (startVertex g') bi
        dstVertices = mapMaybe (endVertex g') bo 
        isNotDstVertex = not . (v `elem`) . mapMaybe (endVertex g') . fromJust . outgoing g'
        isNotStartVertex = not . (v `elem`) . mapMaybe (startVertex g') . fromJust . ingoing g'
    in 
    (sort (allVertices g) == sort (v:allVertices g')) &&
      (all isNotDstVertex srcVertices) && (all isNotStartVertex dstVertices)


-- | Graph class used for graph processing algorithms.
-- A graph processing algorithm does not have to know how the graph is implemented nor if it is
-- directed or undirected
class Graph g where
    -- | Add a new vertex
    addVertex :: Vertex -> b -> g a b -> g a b
    -- | Remove a vertex
    removeVertex :: Vertex -> g a b -> g a b
    -- | Get the vertex value if the vertex is found in the graph
    vertexValue :: g a b -> Vertex -> Maybe b
    -- | Change the vertex value if the vertex is found in the graph
    changeVertexValue :: Vertex -> b -> g a b -> Maybe (g a b)
    -- | Generate a \"random\" vertex
    someVertex :: g a b -> Maybe Vertex

    -- | Check is the graph has no vertrex
    hasNoVertices :: g a b -> Bool

    -- | Generate all vertices
    allVertices :: g a b -> [Vertex]

    -- | Get all the values
    allVertexValues :: g a b -> [b]

    -- | Get all nodes
    allNodes :: g a b -> [(Vertex,b)]

    -- | Check if two vertices are linked by a vertex
    isLinkedWithAnEdge :: g a b -> Vertex -> Vertex -> Bool

    -- | Add an edge
    addEdge :: Edge -> a -> g a b  -> g a b

    -- | Remove an dedge
    removeEdge :: Edge -> g a b -> g a b

    -- | Vertices for an edge
    edgeVertices :: g a b -> Edge -> Maybe (Vertex,Vertex)

    -- | Edge value if the edge is found in the graph
    edgeValue :: g a b -> Edge -> Maybe a

    -- | Return a \"random\" edge
    someEdge :: g a b -> Maybe Edge

    -- | Check if the graph has no edges
    hasNoEdges :: g a b -> Bool

    -- | One extremity of the edge (which is the end only for directed edge)
    endVertex :: g a b -> Edge -> Maybe Vertex
    endVertex g e = do 
      (_,ve) <- edgeVertices g e
      return ve 
        
    -- | One extremity of the edge (which is the start only for directed edge)
    startVertex :: g a b -> Edge -> Maybe Vertex
    startVertex g e = do 
      (vs,_) <- edgeVertices g e
      return vs

    -- | All edges of the graph
    allEdges :: g a b -> [Edge]

    -- | All values of the graph
    allEdgeValues :: g a b -> [a]
   
    -- | Returns an empty graph
    emptyGraph :: g a b

    -- | Check if the graph is empty
    isEmpty :: g a b -> Bool
    isEmpty g = hasNoVertices g && hasNoEdges g

    -- | Check if the graph is oriented
    oriented :: g a b -> Bool

    -- | All the neighbors of a vertex
    neighbors :: g a b -> Vertex -> Maybe [Vertex]

-- | A named graph is a graph where the vertices have a name.
-- This name is not a vertex value. Putting this name in the vertex value
-- would make algorithm less readable.
-- A vertex name is only useful to display the graph.
-- Labeled graph has a different meaning in graph theory.
class Graph g => NamedGraph g where
    -- | Add a vertex with a vertex name in addition to the value
    addLabeledVertex :: String -> Vertex -> b -> g a b -> g a b
    -- | Returns the vertex label
    vertexLabel :: g a b -> Vertex -> Maybe String


-- | Undirected graph
class Graph g => UndirectedGraph g where
    edges :: g a b -> Vertex -> Maybe [Edge]

-- | Directed graph
class Graph g => DirectedGraph g where
    ingoing :: g a b -> Vertex -> Maybe [Edge]
    outgoing :: g a b -> Vertex -> Maybe [Edge]


-- | Return the Markov blanket of a node 
markovBlanket :: DirectedGraph g => g a b -> Vertex -> [Vertex]
markovBlanket g c = 
  let p = parentNodes g c
      children = childrenNodes g c 
      parentOfChildrens = concatMap (parentNodes g) children 
  in 
  nub (c:p ++ children ++ parentOfChildrens)

-- | Return the parents of a node
parentNodes :: DirectedGraph g => g a b -> Vertex -> [Vertex]
parentNodes g v = maybe [] id $ do 
  ie <- ingoing g v
  p <- mapM (startVertex g) ie
  return p

-- | Return the children of a node
childrenNodes :: DirectedGraph g => g a b -> Vertex -> [Vertex]
childrenNodes g v = maybe [] id $ do 
  ie <- outgoing g v
  p <- mapM (endVertex g) ie
  return p

isRoot :: DirectedGraph g => g a b -> Vertex -> Bool
{-# INLINE isRoot #-}
isRoot g v =
  case ingoing g v of 
    Just [] -> True 
    _ -> False

-- | Get the root node for the graph
rootNode :: DirectedGraph g => g a b -> Maybe Vertex
rootNode g = 
  let someRoots = filter (isRoot g) . allVertices $ g
  in 
  case someRoots of 
    (h:l) -> Just h 
    _ -> Nothing

-- | Check if the graph is a directed Acyclic graph
dag :: DirectedGraph g => g a b -> Bool 
dag g = case rootNode g of 
  Nothing -> isEmpty g 
  Just r -> dag (removeVertex r g)

-- | Check if the graph is connected
connectedGraph :: Graph g => g a b -> Bool 
connectedGraph g = 
  let visited = visitVertex g (Set.empty) ([fromJust $ someVertex g])
      vertices = Set.fromList $ allVertices g
      equalSets a b = Set.isSubsetOf a b && Set.isSubsetOf b a
  in 
  equalSets visited vertices
 where 
  visitVertex _ visited [] = visited
  visitVertex theGraph visited (current:n) = 
    if Set.member current visited
      then 
        visitVertex theGraph visited n
      else
        let n' = fromJust $ neighbors theGraph current
        in
        visitVertex theGraph (Set.insert current visited) (n ++ n')



-- | Create an edge description
edge :: Vertex -> Vertex -> Edge 
{-# INLINE edge #-}
edge a b = Edge a b

-- | Endpoints of an edge
edgeEndPoints :: Edge -> (Vertex,Vertex)
edgeEndPoints (Edge va vb) = (va,vb)





-- | Class used to share as much code as possible between
-- directed and undirected graphs without
-- implementing an undirected graph as a graph where
-- we have a directed edge in both directions 
class NeighborhoodStructure n where
  -- | Return an empty neighborhood
  emptyNeighborhood :: n 
  -- | Ingoing edges
  ingoingNeighbors :: n -> [Edge]
  -- | Outgoing edge
  outgoingNeighbors :: n -> [Edge]
  -- | Remove an edge
  removeNeighborsEdge :: Edge -> n -> n
  -- | Add an outgoing edge
  addOutgoingEdge :: Edge -> n -> n
  -- Add in ingoing edge
  addIngoingEdge :: Edge -> n -> n

-- | Directed neighborhood structure for a vertex
instance NeighborhoodStructure DE where
  emptyNeighborhood = DE [] []
  ingoingNeighbors (DE i _) = i
  outgoingNeighbors (DE _ o) = o 
  removeNeighborsEdge e (DE i o) = 
    let i' = filter (/= e) i
        o' = filter (/= e) o 
    in 
    DE i' o'
  addOutgoingEdge e (DE i o) = DE i (e:o)
  addIngoingEdge e (DE i o) = DE (e:i) o

-- | Undirected neighborhood structure for a vertex
instance NeighborhoodStructure UE where
  emptyNeighborhood = UE []
  ingoingNeighbors (UE e) = e
  outgoingNeighbors (UE e) = e
  removeNeighborsEdge e (UE l) = 
    let l' = filter (/= e) l
    in 
    UE l'
  addOutgoingEdge e (UE l) = UE (e:l)
  addIngoingEdge e (UE l) = UE (e:l)



-- | Directed simple graph
type DirectedSG = SimpleGraph DE

-- | Undirected simple graph
type UndirectedSG = SimpleGraph UE

-- | Get the variable name mapping
varMap :: SimpleGraph n e v -> M.Map String Vertex 
varMap (SP _ _ n) = M.fromList . map (\(i,s) -> (s, Vertex i)) . IM.toList $ n

instance (Eq a, Eq b) => Eq (SimpleGraph DE a b) where
    (==) (SP a b _) (SP a' b' _) = a == a' && b == b'

-- | An empty simple graph
emptySimpleGraph = SP M.empty IM.empty IM.empty

-- | Used to prevent adding duplicates to a graph
noRedundancy new old = old

instance FactorContainer (SimpleGraph local edge) where 
   changeFactor = changeFactorInFunctor 

instance Functor (SimpleGraph local edge) where 
  fmap f (SP em vm nm) = SP em (IM.map (\(l,d) -> (l, f d)) vm) nm

class FunctorWithVertex g where 
   fmapWithVertex :: (Vertex -> a -> b) -> g c a -> g c b 
   fmapWithVertexM :: Monad m => (Vertex -> a -> m b) -> g c a -> m (g c b) 


instance FunctorWithVertex (SimpleGraph local) where 
    fmapWithVertex f (SP em vm nm) = SP em (IM.mapWithKey (\k (l,d) -> (l, f (Vertex k) d)) vm) nm
    fmapWithVertexM f (SP em vm nm) = do 
      let l = IM.toList vm
          g f (k,(l,d)) = do 
            r <- f (Vertex k) d 
            return (k,(l,r))
      rl <- mapM (g f) $ l
      let vm' = IM.fromList rl
      return $ SP em vm' nm


instance F.Foldable (SimpleGraph local edge) where
  foldr f c (SP _ vm _) = IM.foldr (\(_,d) s -> f d s) c vm

instance T.Traversable (SimpleGraph local edge) where
  traverse f (SP em vm nm) = 
    let l = IM.toList vm -- [(IM.Key, (DE, String))]
        onTriple f (k,(l,v)) = (\z -> (k,(l,z))) <$> f v
        l' = T.traverse (onTriple f) l -- f [(k,(l,z))]
        result y =  (\x -> SP em (IM.fromList x) nm) <$> y
    in 
    result l'

-- | The foldable class is limited. For a graph g we may need the vertex in addition to the value
class FoldableWithVertex g where
  -- | Fold with vertex 
  foldrWithVertex :: (Vertex -> a -> b -> b) -> b -> g c a -> b 
  foldlWithVertex' :: (b -> Vertex -> a -> b) -> b -> g c a -> b 

instance FoldableWithVertex (SimpleGraph local) where
  foldrWithVertex f s (SP _ vm _) = IM.foldrWithKey (\k (_,v) y -> f (Vertex k) v y) s vm
  foldlWithVertex' f s (SP _ vm _) = IM.foldlWithKey' (\y k (_,v)  -> f y (Vertex k) v) s vm

_addLabeledVertex vertexName vert@(Vertex v) value (SP em vm name) =
  let vm' = IM.insertWith' noRedundancy v (emptyNeighborhood,value) vm
      name' = IM.insert v vertexName name 
  in
  SP em vm' name'

_vertexLabel (SP _ _ name) (Vertex v) = IM.lookup v name

instance NamedGraph DirectedSG where
      addLabeledVertex = _addLabeledVertex
      vertexLabel = _vertexLabel

instance NamedGraph UndirectedSG where
      addLabeledVertex = _addLabeledVertex
      vertexLabel = _vertexLabel

-- | SimpleGraph is an instance of Graph.
instance Graph DirectedSG where
    addVertex = _addVertex
    removeVertex = _removeVertex
    vertexValue = _vertexValue
    changeVertexValue = _changeVertexValue
    someVertex = _someVertex
    hasNoVertices = _hasNoVertices
    allVertices = _allVertices
    allVertexValues = _allVertexValues
    allNodes = _allNodes
    isLinkedWithAnEdge = _isLinkedWithAnEdge
    addEdge = _addEdge
    removeEdge = _removeEdge
    edgeVertices = _edgeVertices
    edgeValue = _edgeValue
    someEdge = _someEdge
    hasNoEdges = _hasNoEdges
    allEdges = _allEdges
    allEdgeValues = _allEdgeValues
    emptyGraph = _emptyGraph
    oriented _ = True
    neighbors g v = nub <$> liftA2 (++) 
             (map (\(Edge _ e) -> e) <$> (outgoing g v)) 
             (map (\(Edge s _) -> s) <$> (ingoing g v))

-- | Reverse edge direction
reverseEdge :: Edge -> Edge 
reverseEdge (Edge va vb) = edge vb va

-- | SimpleGraph is an instance of Graph.
instance Graph UndirectedSG where
    addVertex = _addVertex
    removeVertex = _removeVertex
    vertexValue = _vertexValue
    changeVertexValue = _changeVertexValue
    someVertex = _someVertex
    hasNoVertices = _hasNoVertices
    allVertices = _allVertices
    allVertexValues = _allVertexValues
    allNodes = _allNodes
    isLinkedWithAnEdge = _isLinkedWithAnEdge
    addEdge = _addEdge
    removeEdge e g = _removeEdge (reverseEdge e) (_removeEdge e g)
    edgeVertices = _edgeVertices
    edgeValue g e = case _edgeValue g e of 
                       Nothing -> _edgeValue g (reverseEdge e) 
                       r@(Just _) -> r
    someEdge = _someEdge
    hasNoEdges = _hasNoEdges
    allEdges = _allEdges
    allEdgeValues = _allEdgeValues
    emptyGraph = _emptyGraph
    oriented _ = False
    -- in undirected graphs the edge direction does not count so we need to get both
    -- ends to be sure we don not forget a vertex. In addition to that, an end may be the current vertex which
    -- is not part of the neighbors. So it has to be filtered out. Obviously, a better solution will
    -- have to be designed.
    neighbors g v = filter (/= v) <$> nub <$> liftA2 (++) 
       (map (\(Edge _ e) -> e) <$> (edges g v)) 
       (map (\(Edge s _) -> s) <$> (edges g v))

_emptyGraph = emptySimpleGraph

_hasNoVertices (SP _ vm _) = IM.null vm

_hasNoEdges (SP em _ _) = M.null em

_allVertices (SP _ vm _) = map Vertex . IM.keys $ vm

_allEdges (SP em _ _) = M.keys $ em

_allNodes (SP _ vm _) = map (\(k,(_,v)) -> (Vertex k,v)) . IM.assocs $ vm

_allVertexValues (SP _ vm _) = map snd (IM.elems vm)

_allEdgeValues (SP em _ _) = M.elems em

_isLinkedWithAnEdge :: SimpleGraph n e v -> Vertex -> Vertex -> Bool 
{-# INLINE _isLinkedWithAnEdge #-}
_isLinkedWithAnEdge (SP em _ _) va vb = M.member (edge va vb) em || M.member (edge vb va) em

_someVertex (SP _ vm _) = 
  if IM.null vm 
    then 
      Nothing 
    else 
      Just . Vertex . head . IM.keys $ vm

_someEdge (SP em _ _) = 
  if M.null em 
    then 
      Nothing 
    else 
      Just . head . M.keys $ em

_addVertex vert@(Vertex v) value (SP em vm nm) = SP em (IM.insertWith' noRedundancy v (emptyNeighborhood,value) vm) nm

_removeVertex v@(Vertex vertex) g@(SP _ vm _)  = maybe g removeVertexWithValue $! (IM.lookup vertex vm)
  where
    removeVertexWithValue (n,_) = let g' = foldr _removeEdge g (ingoingNeighbors n)
                                      SP em vm' nm' = foldr _removeEdge g' (outgoingNeighbors n)
                                  in 
                                  SP em (IM.delete vertex vm') nm'
_vertexValue g@(SP _ vm _) (Vertex i) = maybe Nothing (Just . extractValue) $! (IM.lookup i vm)
  where
    extractValue (_,d) = d

_changeVertexValue v@(Vertex vi) newValue g@(SP e vm nm)  = 
  let newVertexMap = do
       (n,_) <- IM.lookup vi vm
       return $ IM.insert vi (n,newValue) vm
  in 
  case newVertexMap of 
    Nothing -> Just g
    Just nvm -> Just $ SP e nvm nm

_removeEdge e@(Edge (Vertex vs) (Vertex ve)) g@(SP em vm nm)  = 
  let r = do 
        _ <- M.lookup e em -- Check e is member of the graph
        (ns,vsdata) <- IM.lookup vs vm
        (ne,vedata) <- IM.lookup ve vm
        return ((vs,(removeNeighborsEdge e ns,vsdata)),(ve,(removeNeighborsEdge e ne,vedata)))
      updateGraph ((vs,vsdata),(ve,vedata)) =
        let vm' = IM.insert ve vedata . IM.insert vs vsdata $ vm
            em' = M.delete e em 
        in 
        SP em' vm' nm
  in 
  maybe g updateGraph r

_edgeVertices (SP em _ _) e@(Edge vs ve) =
     if M.member e em 
      then 
        Just (vs,ve)
      else
        Nothing

_edgeValue :: SimpleGraph n e v -> Edge -> Maybe e 
{-# INLINE _edgeValue #-}
_edgeValue (SP em _ _) e = do
     v <- M.lookup e em
     return v

addEdgeReference :: NeighborhoodStructure local 
                 => Edge 
                 -> IM.IntMap (local, vertexdata) 
                 -> Vertex 
                 -> Vertex 
                 -> IM.IntMap (local, vertexdata)
{-# INLINE addEdgeReference #-}
addEdgeReference newEdge vm (Vertex vsi) (Vertex vei) = id $! IM.adjust addi vei $! (IM.adjust addo vsi vm)
 where
   addi (n,v) = (addIngoingEdge newEdge n,v)  
   addo (n,v) = (addOutgoingEdge newEdge n,v)  

_addEdge :: (NeighborhoodStructure n,Graph (SimpleGraph n)) => Edge -> e -> SimpleGraph n e v -> SimpleGraph n e v 
{-# INLINE _addEdge #-}
_addEdge newEdge@(Edge vs ve) value g@(SP em vm nm)   = 
  if testEdgeExistence g em vs ve 
    then 
      g
    else
      SP (M.insert newEdge value em) (addEdgeReference newEdge vm vs ve) nm
  where
    testEdgeExistence g em va vb = 
      if (oriented g)
        then 
          M.member (Edge va vb) em
        else 
          M.member (Edge va vb) em || M.member (Edge vb va) em 
    

instance UndirectedGraph UndirectedSG where
  edges g@(SP _ vm _) v@(Vertex vi) =
      do 
        (n,_) <- IM.lookup vi vm
        return (ingoingNeighbors n)

instance DirectedGraph DirectedSG where
  ingoing g@(SP _ vm _) v@(Vertex vi) =
      do 
        (n,_) <- IM.lookup vi vm
        return (ingoingNeighbors n)

  outgoing g@(SP _ vm _) v@(Vertex vi) =
      do 
        (n,_) <- IM.lookup vi vm
        return (outgoingNeighbors n) 

{-
 
Following code is used to display a graph in a form adapted to humans.

-}

bracketS :: String -> String 
bracketS [] = []
bracketS s = " [" ++ s ++ "];"

createNodeStyle :: (MonadWriter String m) 
                => (Vertex -> n -> Maybe String)
                -> (Vertex -> n -> Maybe String)
                -> Maybe String 
                -> Vertex 
                -> n 
                -> m ()
createNodeStyle nodeShape nodeColor maybeLabel v n = 
  let apply f = f v n
      label _ _ = case maybeLabel of 
         Nothing -> Nothing 
         Just s -> Just $ "label=\"" ++ s ++ "\""
  in 
  tell $ bracketS . intercalate "," . mapMaybe apply $ [nodeShape,nodeColor, label]
  

createEdgeStyle :: (MonadWriter String m) 
                => (Edge -> e -> Maybe String)
                -> (Edge -> e -> Maybe String)
                -> Edge
                -> e 
                -> m ()
createEdgeStyle edgeShape edgeColor e n = 
  let apply f = f e n
  in 
  tell $ bracketS . intercalate "," . mapMaybe apply $ [edgeShape,edgeColor]



printNode nm (Vertex k,v) = do 
  tell "\n"
  let r = IM.lookup k nm
  when (isJust r) $ do
     tell $ "Node " ++ fromJust r
  tell "\n"
  tell $ show v
  tell "\n"

addVertexToGraphviz nodeShape nodeColor nm (k,(_,v)) = do
  tell $ show k
  let r = IM.lookup k $ nm 
  createNodeStyle nodeShape nodeColor r (Vertex k) v
  tell "\n"

addVertexToUndirectedGraphviz nm (k,(_,v)) = do
  tell $ show k
  tell "\n"

-- | Print the values of the graph vertices
printGraphValues :: (Graph (SimpleGraph n), Show b) => SimpleGraph n e b -> IO () 
printGraphValues g@(SP _ _ nm) = putStrLn . execWriter $ mapM_ (printNode nm) (allNodes g)

displaySimpleGraph :: (Vertex -> n -> Maybe String)
                   -> (Vertex -> n -> Maybe String)
                   -> (Edge -> e -> Maybe String)
                   -> (Edge -> e -> Maybe String)
                   -> SimpleGraph local e n 
                   -> String 
displaySimpleGraph  nodeShape nodeColor edgeShape edgeColor g@(SP em vm nm) = execWriter $ do 
  tell "digraph dot {\n"
  mapM_ (addVertexToGraphviz nodeShape nodeColor  nm) $ IM.toList vm
  tell "\n"
  mapM_ (addEdgeToGraphviz edgeShape edgeColor ) $ M.toList em
  tell "}\n"
   where
     addEdgeToGraphviz es ec (e@(Edge (Vertex vs) (Vertex ve)),l) = do
       tell $ show vs 
       tell " -> "
       tell $ show ve
       createEdgeStyle es ec e l
       tell "\n"  

noNodeStyle _ _ = Nothing 
noEdgeStyle _ _ = Nothing

instance Show (DirectedSG () CPT) where
  show g = displaySimpleGraph noNodeStyle noNodeStyle noEdgeStyle noEdgeStyle g

instance Show (DirectedSG () MAXCPT) where
  show g = displaySimpleGraph noNodeStyle noNodeStyle noEdgeStyle noEdgeStyle g

instance Show (DirectedSG String String) where
  show g = displaySimpleGraph noNodeStyle noNodeStyle noEdgeStyle noEdgeStyle g

instance (Show b, Show e) => Show (UndirectedSG e b)where
  show g@(SP em vm nm) = execWriter $ do
    tell "graph dot {\n"
    mapM_ (addVertexToUndirectedGraphviz nm) $ IM.toList vm
    tell "\n"
    mapM_ (addEdgeToGraphviz) $ M.toList em
    tell "}\n"
     where
       addEdgeToGraphviz (e@(Edge (Vertex vs) (Vertex ve)),l) = do
         tell $ show vs 
         tell " -- "
         tell $ show ve
         tell "\n"


displayFactors :: (NeighborhoodStructure n, Show f, Factor f, Graph (SimpleGraph n)) => SimpleGraph n a f -> String
displayFactors g@(SP _ _ nm) = 
  let nodes = allNodes g
      displayFactor (Vertex i,f) = 
        let s = fromJust . IM.lookup i $ nm
        in
        s ++ "\n" ++ show f
  in
  intercalate "\n" $ map displayFactor nodes

{-

Graph Monad

-}



-- | State used for the construction of the graph in the monad and containing
-- auxiliary informations like vertex name to vertex id and vertex count
type AuxiliaryState = (M.Map String Int, Int)

emptyAuxiliaryState = (M.empty,0)

-- | The state of the graph monad : the graph and auxiliary data
-- useful during the construction
type GMState g e f = (AuxiliaryState,g e f)

-- | Graph monad.
-- The monad used to simplify the description of a new graph
-- g is the graph type. e the edge type. f the node type (generally a 'Factor')
newtype GraphMonad g e f a = GM {runGraphMonad :: State (GMState g e f) a} deriving(Functor,Applicative,Monad, MonadState (GMState g e f))

-- | Get a named vertex from the graph monad
getVertex :: Graph g => String -> GraphMonad g e f (Maybe Vertex)
getVertex a = do
  (namemap,_) <- gets fst
  return $ do
    i <- M.lookup a namemap
    return (Vertex i)

-- | Add a new labeled edge to the graph
newEdge :: Graph g => Vertex -> Vertex -> e -> GraphMonad g e f ()
newEdge va vb e = do
  (aux,g) <- get 
  let g1 = addEdge (edge va vb) e g
  put $! (aux,g1)
  return ()

-- | Add a node in the graph using the graph monad
graphNode :: NamedGraph g => String -> f -> GraphMonad g e f Vertex 
graphNode vertexName initValue = do
  ((namemap,_),_) <- get
  maybe (getNewEmptyVariable (Just vertexName) initValue) returnVertex $! (M.lookup vertexName namemap)
   where
    returnVertex i = return (Vertex i)

-- | Generate a new unique unamed empty variable
getNewEmptyVariable :: NamedGraph g => Maybe String -> f -> GraphMonad g e f Vertex  
getNewEmptyVariable name initValue = do 
  ((namemap,count),g) <- get 
  let vertexName = maybe ("unamed" ++ show count) id name
      g1 = addLabeledVertex vertexName (Vertex count) initValue g
      namemap1 = M.insert vertexName count namemap
  put $! ((namemap1,count+1),g1)
  return (Vertex count)

runGraph :: Graph g => GraphMonad g e f a -> (a,g e f)
runGraph = removeAuxiliaryState . flip runState (emptyAuxiliaryState,emptyGraph) . runGraphMonad 
 where 
  removeAuxiliaryState (r,(_,g)) = (r,g)

evalGraph :: Graph g => GraphMonad g e f a -> a
evalGraph = flip evalState (emptyAuxiliaryState,emptyGraph) . runGraphMonad 

execGraph :: Graph g => GraphMonad g e f a -> g e f
execGraph = snd . flip execState (emptyAuxiliaryState,emptyGraph) . runGraphMonad 
