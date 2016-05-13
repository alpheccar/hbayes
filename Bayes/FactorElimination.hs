{- | Algorithms for factor elimination

-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bayes.FactorElimination(
    -- * Moral graph
      moralGraph
    -- * Triangulation
    , nodeComparisonForTriangulation
    , numberOfAddedEdges
    , weight
    , weightedEdges
    , triangulate
    -- * Junction tree
    , createClusterGraph
    , Cluster
    , createJunctionTree
    , createUninitializedJunctionTree
    , JunctionTree
    , displayTreeValues
    -- * Shenoy-Shafer message passing
    , collect 
    , distribute
    , posterior 
    -- * Evidence
    , changeEvidence
    -- * Test 
    , junctionTreeProperty_prop
    , junctionTreeAllClusters_prop
    , VertexCluster
    -- * For debug 
    , junctionTreeProperty
    , maximumSpanningTree
    , fromVertexCluster
    , triangulatedebug
    ) where

import Bayes
import qualified Data.Foldable as F
import Data.Maybe(fromJust,mapMaybe,isJust)
import Control.Monad(mapM,guard)
import Bayes.Factor hiding (isEmpty)
import Data.Function(on)
import Data.List(minimumBy,maximumBy,inits,foldl',nub,(\\))
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Functor as Functor
import qualified Data.Tree as T 
import Bayes.FactorElimination.JTree
import Control.Applicative((<$>))
import Bayes.VariableElimination(marginal)

import Test.QuickCheck hiding ((.||.), collect)
import Test.QuickCheck.Arbitrary
import Bayes.VariableElimination.Buckets(IsBucketItem(..))


import Bayes.Factor.CPT -- This import is only used for quickcheck tests

--import Debug.Trace
--debug s a = trace (s ++ "\n" ++ show a ++ "\n") a

{-
 
Comparison functions for graph triangulation

-}

-- | Number of edges added when connecting all neighbors
numberOfAddedEdges :: UndirectedGraph g 
                   => g a b 
                   -> Vertex 
                   -> Integer 
numberOfAddedEdges g v = 
    let nodes = fromJust $ neighbors g v
    in 
    fromIntegral $ length [edge x y | x <- nodes, y <- nodes, x /= y, not (isLinkedWithAnEdge g x y)]

weightedEdges :: (UndirectedGraph g, Factor f) 
              => g a f 
              -> Vertex 
              -> Integer 
weightedEdges g v = 
    let nodes = fromJust $ neighbors g v
    in 
    sum [weight g x * weight g y | x <- nodes, y <- nodes, x /= y, not (isLinkedWithAnEdge g x y)]

-- | Weight of a node
weight :: (UndirectedGraph g, Factor f)
       => g a f 
       -> Vertex 
       -> Integer 
weight g v = 
    fromIntegral $ factorDimension . fromJust . vertexValue g $ v

(.||.) :: (a -> a -> Ordering)
       -> (a -> a -> Ordering) 
       -> (a -> a -> Ordering)
f .||. g = 
    \a b -> case f a b of
              EQ -> g a b 
              r -> r

-- | Node selection comparison function used for triangulating the graph
nodeComparisonForTriangulation :: (UndirectedGraph g, Factor f)
                               => g a f
                               -> Vertex 
                               -> Vertex 
                               -> Ordering 
nodeComparisonForTriangulation g = (compare `on` (weightedEdges g))

{-

Graph triangulation

-}

-- | A cluster containing only the vertices and not yet the factors
newtype VertexCluster = VertexCluster (Set.Set Vertex) deriving(Eq,Ord)

fromVertexCluster (VertexCluster s) = s

instance Show VertexCluster where 
    show (VertexCluster s) = show . Set.toList $ s

-- | Triangulate a graph using a cost function
-- The result is the triangulated graph and the list of clusters
-- which may not be maximal.
triangulate :: Graph g
            => (Vertex -> Vertex -> Ordering) -- ^ Criterion function for triangulation
            -> g () b
            -> [VertexCluster] -- ^ Returns the clusters and the triangulated graph
triangulate cmp gr = removeNodes cmp gr []
 where 
  removeNodes cmp g l | hasNoVertices g = keepMaximalClusters (reverse l)
                      | otherwise = 
                          let selectedNode = minimumBy cmp (allVertices g)
                              theNeighbors = fromJust $ neighbors g selectedNode
                              g' = removeVertex selectedNode . connectAllNonAdjacentNodes theNeighbors $ g 
                              newCluster = VertexCluster . Set.fromList $ (selectedNode:theNeighbors)
                          in 
                          removeNodes cmp g' (newCluster:l)

triangulatedebug :: Graph g
            => (Vertex -> Vertex -> Ordering) -- ^ Criterion function for triangulation
            -> g () b
            -> ([VertexCluster],[g () b]) -- ^ Returns the clusters and the triangulated graph
triangulatedebug cmp gr = removeNodes cmp gr [] []
 where 
  removeNodes cmp g l gl | hasNoVertices g = (reverse l,reverse gl)
                         | otherwise = 
                             let selectedNode = minimumBy cmp (allVertices g)
                                 theNeighbors = fromJust $ neighbors g selectedNode
                                 g' = removeVertex selectedNode . connectAllNonAdjacentNodes theNeighbors $ g 
                                 newCluster = VertexCluster . Set.fromList $ (selectedNode:theNeighbors)
                             in 
                             removeNodes cmp g' (newCluster:l) (g:gl)


-- | Find for a containing cluster. 
findContainingCluster :: VertexCluster -- ^ Cluster processed
                      -> [VertexCluster] -- ^ Cluster list where to look for a containing cluster
                      -> (Maybe VertexCluster,[VertexCluster]) -- ^ Return the containing cluster and a new list without the containing cluster
findContainingCluster cluster l = 
  let  clusterIsNotASubsetOf s = (Set.isSubsetOf (fromVertexCluster cluster) (fromVertexCluster s))
       (prefix,suffix) = break clusterIsNotASubsetOf l
  in 
  case suffix of 
    [] -> (Nothing,l)
    _ -> (Just (head suffix),prefix ++ tail suffix)


-- | Remove clusters already contained in a previous clusters
keepMaximalClusters :: [VertexCluster] -> [VertexCluster]
keepMaximalClusters [] = []
keepMaximalClusters l = checkIfMaximal [] (head l) (tail l)
 where 
  checkIfMaximal reversedPrefix current [] = 
    case findContainingCluster current (reverse reversedPrefix) of 
      (Nothing,_) -> reverse (current:reversedPrefix) 
      (Just r,l) -> reverse (r:reverse l)
  checkIfMaximal reversedPrefix current suffix = 
    case findContainingCluster current (reverse reversedPrefix) of 
      (Nothing,_) -> checkIfMaximal (current:reversedPrefix) (head suffix) (tail suffix)
      (Just r,l) -> checkIfMaximal (r:reverse l) (head suffix) (tail suffix)

-- | Convert the clusters from vertex to 'DV' clusters
vertexClusterToCluster :: (Factor f , Graph g)
                       => g e f 
                       -> VertexCluster 
                       -> Cluster 
vertexClusterToCluster g c = 
  let vertices = Set.toList . fromVertexCluster $ c
      variables = map factorMainVariable . mapMaybe (vertexValue g) $ vertices
  in 
  Cluster . Set.fromList $ variables


-- | Create the cluster graph
createClusterGraph :: (UndirectedGraph g, Factor f, Graph g')
                   => g' e f
                   -> [VertexCluster] 
                   -> g Int Cluster
createClusterGraph bn c =
  let numberedClusters = zip c (map Vertex [0..])
      addCluster g (c,v)  = addVertex v (vertexClusterToCluster bn c) g
      graphWithoutEdges = foldl' addCluster emptyGraph numberedClusters
      separatorSize ca cb = Set.size $ Set.intersection (fromVertexCluster ca) (fromVertexCluster cb)
      allEdges = [(cx,cy) | cx <- numberedClusters, cy <- numberedClusters, cx /= cy]
      addClusterEdge g ((ca,va),(cb,vb)) = addEdge (edge va vb) (separatorSize ca cb) g
  in 
  foldl' addClusterEdge graphWithoutEdges allEdges


{-

Maximum spanning tree using Prim's algorithm
  
-}

-- | Get all possible edges between the leaves and the remaining nodes
possibilities :: (Ord c , UndirectedGraph g) 
              => g Int c -- ^ Original graph to get the edge value 
              -> JTree c f -- ^ Tree to get the vertex for a leaf
              -> [Vertex] -- ^ Vertices to add to the tree
              -> [c] -- ^ List of leaves
              -> [(Vertex,c,Int)] -- ^ Found edge to add
possibilities g currentT remaining leavesClusters = do 
  rv <- remaining
  lv <- leavesClusters
  let NodeValue lvVertex _ _ = nodeValue currentT lv
  guard (isLinkedWithAnEdge g rv lvVertex)
  let ev = fromJust $ edgeValue g (edge rv lvVertex)
  return $ (rv,lv,ev)

-- | Find the max edge to add to the tree
findMax :: (UndirectedGraph g, Ord c, Factor f,Show c)
        => g Int c -- ^ Graph
        -> [Vertex] -- ^ Nodes to add 
        -> JTree c f
        -> ([Vertex],(Vertex,c),c) 
findMax g remaining currentT = 
  let leavesClusters = treeNodes currentT
      edgeValue (_,_,e) = e
      (rf,lf,ef) = maximumBy (compare `on` edgeValue) (possibilities g currentT remaining leavesClusters)
      remaining' = filter (/= rf) remaining 
      foundCluster = fromJust $ vertexValue g rf
  in
  (remaining', (rf, foundCluster), lf)

-- | Implementing the Prim's algorithm for minimum spanning tree
maximumSpanningTree :: (UndirectedGraph g, IsCluster c, Factor f, Ord c, Show c, Show f) 
                    => g Int c 
                    -> JTree c f
maximumSpanningTree g = 
    let rootNodeVertex = fromJust $ someVertex g 
        rootNodeValue = fromJust $ vertexValue g rootNodeVertex
        startTree = singletonTree rootNodeValue rootNodeVertex [] [] 
        remainingVertices = filter (/= rootNodeVertex) (allVertices g) 
    in 
    buildTree g remainingVertices startTree 

buildTree :: (UndirectedGraph g , IsCluster c, Factor f, Ord c, Show c, Show f)
          => g Int c 
          -> [Vertex]
          -> JTree c f 
          -> JTree c f
buildTree g [] currentT = currentT 
buildTree g l currentT = 
    let (l',(foundElemVertex,foundElemValue),leaf) = findMax g l currentT
        sep = mkSeparator foundElemValue leaf
        newTree = addSeparator leaf sep foundElemValue . 
                  addNode foundElemValue foundElemVertex [] [] $ currentT
    in 
    buildTree g l' newTree
   
{-

Junction tree algorithm

-}


-- | Create a junction tree with only the clusters and no factors
createUninitializedJunctionTree :: (DirectedGraph g, FoldableWithVertex g, NamedGraph g, Factor f, Show f)
                                => (UndirectedSG () f -> Vertex -> Vertex -> Ordering) -- ^ Weight function on the moral graph
                                -> g () f -- ^ Input directed graph
                                -> JunctionTree f -- ^ Junction tree
createUninitializedJunctionTree cmp g =  
  let theMoralGraph = moralGraph g
      clusters = triangulate (cmp theMoralGraph) theMoralGraph
      g'' = createClusterGraph g clusters :: UndirectedSG Int Cluster
  in 
  maximumSpanningTree g''

-- | Create a function tree
createJunctionTree :: (DirectedGraph g, FoldableWithVertex g, NamedGraph g, Factor f, IsBucketItem f, Show f)
                  => (UndirectedSG () f -> Vertex -> Vertex -> Ordering) -- ^ Weight function on the moral graph
                  -> BayesianNetwork g f -- ^ Input directed graph
                  -> JunctionTree f -- ^ Junction tree
createJunctionTree cmp g = 
  let cTree = createUninitializedJunctionTree cmp g 
      -- A vertex is linked with a factor so vertex is used as the identifier
      newTree = setFactors g cTree
  in 
  distribute . collect $ newTree


-- | Compute the marginal posterior (if some evidence is set on the junction tree)
-- otherwise compute just the marginal prior. The set of variables must be included inside a cluster
-- for thr algorithm to work. So, most of the cases, it will be used to compute the posterior of just
-- one variable.
posterior :: (BayesianDiscreteVariable dv, Factor f, IsBucketItem f) => JunctionTree f -> [dv] -> Maybe f
posterior t someDv = 
  let v = map dv someDv
  in
  case snd $ traverseTree (findClusterFor v) Nothing t of 
    Nothing -> Nothing
    Just c -> let NodeValue ver f e = nodeValue t c 
                  d = maybe (factorFromScalar 1.0) id $ downMessage t =<< (nodeParent t c)
                  u = map (upMessage t) (nodeChildren t c)
                  allFactors = d:u ++ f ++ e
                  variablesToRemove = (nub (concatMap factorVariables allFactors)) \\ v
                  unNormalized = marginal allFactors variablesToRemove v []
              in 
              Just $ factorDivide unNormalized (factorNorm unNormalized)

-- | Find a cluster containing the variable
findClusterFor :: [DV]
               -> Maybe Cluster
               -> Cluster -- ^ Current cluster
               -> NodeValue f -- ^ Current value
               -> Action (Maybe Cluster) (NodeValue f)
findClusterFor dv s c@(Cluster sc) v = 
  case Set.isSubsetOf (Set.fromList dv) sc of 
    False -> Skip s 
    True -> Stop (Just c)


junctionTreeProperty_prop :: DirectedSG () CPT -> Property 
junctionTreeProperty_prop g = (not . isEmpty) g && (not . hasNoEdges) g && connectedGraph g ==> 
  let cmp ug = (compare `on` (numberOfAddedEdges ug))
      t = createUninitializedJunctionTree cmp g
  in
  junctionTreeProperty t [] (root t)

junctionTreeAllClusters_prop :: DirectedSG () CPT -> Property 
junctionTreeAllClusters_prop g = (not . isEmpty) g && (not . hasNoEdges) g && connectedGraph g ==> 
      let theMoralGraph = moralGraph g
          cmp ug = (compare `on` (numberOfAddedEdges ug))
          clusters = triangulate (cmp theMoralGraph) theMoralGraph
          g'' = createClusterGraph g clusters :: UndirectedSG Int Cluster
          jt = maximumSpanningTree g'' :: JunctionTree CPT
          treeClusters = treeNodes jt 
          sa = Set.fromList (map (vertexClusterToCluster g) clusters) 
          sb = Set.fromList treeClusters 
      in 
      Set.isSubsetOf sa sb && Set.isSubsetOf sb sa

junctionTreeProperty :: JTree Cluster CPT -> [Cluster] -> Cluster -> Bool
junctionTreeProperty t path c = 
  let cl = map (separatorChild t) . nodeChildren t $ c
  in
  checkPath c path && all (junctionTreeProperty t (c:path)) cl 


-- | Check that the intersection of C with any parent in included in all cluster between the parent and C.
checkPath :: Cluster -> [Cluster] -> Bool 
checkPath _ [] = True
checkPath (Cluster c) l = 
  let clusterSet (Cluster s) = s -- x
      parentSets = map clusterSet l -- Example a b c d where a is the root
      allIntersectionsWithParents = map (Set.intersection c) parentSets -- a ^ x, b ^ x , c ^ x , d ^ x
      pathsToEachParent = tail . inits $ parentSets -- a, ab, abc, abcd
      isSubsetOfAllParents i path = all (Set.isSubsetOf i) path
  in    
  and $ zipWith isSubsetOfAllParents allIntersectionsWithParents pathsToEachParent
{-

Moral graph

-}
-- | Get the parents of a vertex
parents :: DirectedGraph g => g a b -> Vertex -> [Vertex]
parents g v = fromJust $ ingoing g v >>= mapM (startVertex g) 

-- | Get the children of a vertex
children :: DirectedGraph g => g a b -> Vertex -> [Vertex]
children g v = fromJust $ outgoing g v >>= mapM (endVertex g) 

-- | Connect all the nodes which are not connected and apply the function f for each new connection
-- The origin and dest graph must share the same vertex.
connectAllNonAdjacentNodes :: (Graph g) 
                           => [Vertex]  -- ^ List of nodes to connect
                           -> g () b -- ^ Graph containing the nodes
                           -> g () b
connectAllNonAdjacentNodes nodes originGraph   =  
    let addEmptyEdge g e = addEdge e () g
    in 
    foldl' addEmptyEdge originGraph [edge x y | x <- nodes, y <- nodes, x /= y, not (isLinkedWithAnEdge originGraph x y)]
   
-- | Add the missing parent links
addMissingLinks :: DirectedGraph g => g () b -> Vertex -> b -> g () b
addMissingLinks g v _ = connectAllNonAdjacentNodes (parents g v) g 


-- | Convert the graph to an undirected form
convertToUndirected :: (FoldableWithVertex g, Graph g, NamedGraph g, NamedGraph g',UndirectedGraph g')
                    => g  () b 
                    -> g' () b 
convertToUndirected m = 
    let addVertexWithLabel g v dat  = 
           let theName = fromJust $ vertexLabel m v
           in 
           addLabeledVertex theName v dat g
        newDiscreteGraph = foldlWithVertex' addVertexWithLabel emptyGraph m
        addEmptyEdge edge g = addEdge edge () g
    in 
    foldr addEmptyEdge newDiscreteGraph . allEdges $ m

-- | For the junction tree construction, only the vertices are needed during the intermediate steps.
-- So, the moral graph is returned without any vertex data.
moralGraph :: (NamedGraph g, FoldableWithVertex g, DirectedGraph g) 
           => g () b -> UndirectedSG () b 
moralGraph g = 
    convertToUndirected  . foldlWithVertex' addMissingLinks g $ g
