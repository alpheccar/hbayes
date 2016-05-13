{- | Junction Trees 

The Tree data structures are not working very well with message passing algorithms. So, junction trees are using
a different representation

-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bayes.FactorElimination.JTree(
      IsCluster(..)
    , Cluster(..)
    , JTree(..)
    , JunctionTree(..)
    , Sep
    , setFactors
    , distribute 
    , collect
    , fromCluster
    , changeEvidence
    , nodeIsMemberOfTree
    , singletonTree
    , addNode 
    , addSeparator
    , leaves
    , nodeValue
    , NodeValue(..)
    , SeparatorValue(..)
    , downMessage
    , upMessage 
    , nodeParent 
    , nodeChildren
    , traverseTree
    , separatorChild
    , treeNodes
    , treeValues
    , displayTreeValues
    , Action(..)
    ) where 

import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Data.Maybe(fromJust,mapMaybe)
import qualified Data.Set as Set
import Data.Monoid
import Data.List((\\), intersect,partition, foldl',minimumBy,nub)
import Bayes.PrivateTypes 
import Bayes.Factor
import Bayes
import Data.Function(on)
import Bayes.VariableElimination(marginal)
import Data.Binary
import Bayes.VariableElimination.Buckets(IsBucketItem(..))

--import Debug.Trace 
--debug s a = trace (s ++ " " ++ show a ++ "\n") a

type UpMessage a = a 
type DownMessage a = Maybe a

-- | Separator value
data SeparatorValue a = SeparatorValue !(UpMessage a) !(DownMessage a)
                      | EmptySeparator -- ^ Use to track the progress in the collect phase
                      deriving(Eq)

instance Show a => Show (SeparatorValue a) where 
    show EmptySeparator = ""
    show (SeparatorValue u Nothing) = "u(" ++ show u ++ ")"
    show (SeparatorValue u (Just d)) = "u(" ++ show u ++ ") d(" ++ show d ++ ")"

type FactorValues a = [a]
type EvidenceValues a = [a]

-- | Node value
data NodeValue a = NodeValue !Vertex !(FactorValues a) !(EvidenceValues a) deriving(Eq)

instance Show a => Show (NodeValue a) where 
    show (NodeValue v f e) = "f(" ++ show f ++ ") e(" ++ show e ++ ")"

newtype Sep = Sep Int deriving(Eq,Ord,Show,Num,Binary)

-- | Junction tree.
-- 'c' is the node / separator identifier (for instance a set of 'DV')
-- a are the values for a node or separator
-- Cluster are unique sor the cluster value is also the cluster key
-- Separator values are not unique. Two different seperators can be the same
-- cluster. So, separator unicity is enforced with a number
data JTree  c f = JTree {  root :: !c
                        -- | Leaves of the tree
                        ,  leavesSet :: !(Set.Set c)
                        -- | The children of a node are separators
                        ,  childrenMap :: !(Map.Map c [Sep])
                        -- | Parent of a node
                        ,  parentMap :: !(Map.Map c Sep)
                        -- | Parent of a separator
                        ,  separatorParentMap :: !(Map.Map Sep c)
                        -- | The child of a seperator is a node
                        ,  separatorChildMap :: !(Map.Map Sep c)
                        -- | Values for nodes and seperators
                        ,  nodeValueMap :: !(Map.Map c (NodeValue f))
                        ,  separatorValueMap :: !(Map.Map Sep (SeparatorValue f))
                        ,  separatorCurrentKey :: !Sep
                        ,  separatorClusterMap :: !(Map.Map Sep c)
                        } deriving(Eq)

instance FactorContainer (JTree Cluster) where 
  changeFactor f t = 
    let changeNodeValue (NodeValue v fa ev) = NodeValue v (changeFactor f fa) ev
    in
    distribute .
    collect $
    t { nodeValueMap = Map.map changeNodeValue (nodeValueMap t)
      , separatorValueMap = Map.map (const EmptySeparator) (separatorValueMap t)
      }

-- | Create a singleton tree with just one root node
singletonTree r rootVertex factorValue evidenceValue = 
    let t = JTree r Set.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty (Sep 0) Map.empty
    in 
    addNode r rootVertex factorValue evidenceValue t

-- | Reset all evidences to 1 in the network
resetEvidences :: Factor f => JTree c f -> JTree c f 
resetEvidences t = t {nodeValueMap = Map.map resetNodeEvidence (nodeValueMap t)}
 where 
  resetNodeEvidence (NodeValue v f _) = NodeValue v f []

-- | Get the cluster for a separator
separatorCluster :: JTree c a -> Sep -> c 
{-# INLINE separatorCluster #-}
separatorCluster t s = fromJust $! Map.lookup s (separatorClusterMap t)

-- | Leaves of the tree
leaves :: JTree c a -> [c]
{-# INLINE leaves #-}
leaves = Set.toList . leavesSet

-- | All nodes of the tree
treeNodes :: JTree c a -> [c]
{-# INLINE treeNodes #-}
treeNodes = Map.keys . nodeValueMap

treeValues :: JTree c f -> [(c,NodeValue f)]
{-# INLINE treeValues #-}
treeValues = Map.toList . nodeValueMap

-- | Value of a node
nodeValue :: Ord c => JTree c a -> c -> NodeValue a 
{-# INLINE nodeValue #-}
nodeValue t e = fromJust $! Map.lookup e (nodeValueMap t)

-- | Change the value of a node
setNodeValue :: Ord c => c -> NodeValue a -> JTree c a -> JTree c a
{-# INLINE setNodeValue #-}
setNodeValue c v t = t {nodeValueMap = Map.insert c v (nodeValueMap t)} 

-- | Parent of a node
nodeParent :: Ord c => JTree c a -> c -> Maybe Sep 
{-# INLINE nodeParent #-}
nodeParent t e = let r = Map.lookup e (parentMap t) in r `seq` r

-- | Value of a node
separatorValue :: Ord c => JTree c a -> Sep -> SeparatorValue a 
{-# INLINE separatorValue #-}
separatorValue t e = fromJust $! Map.lookup e (separatorValueMap t)

-- | Parent of a separator
separatorParent :: Ord c => JTree c a -> Sep -> c 
{-# INLINE separatorParent #-}
separatorParent t e = fromJust $! Map.lookup e (separatorParentMap t)

-- | UpMessage for a separator node
upMessage :: Ord c => JTree c a -> Sep -> a
upMessage t c = case separatorValue t c of 
                  SeparatorValue up _ -> up 
                  _ -> error "Trying to get an up message on an empty seperator ! Should never occur !"

-- | DownMessage for a separator node
downMessage :: Ord c => JTree c a -> Sep -> Maybe a 
downMessage t c = case separatorValue t c of 
     SeparatorValue _ (Just down) -> Just down 
     SeparatorValue _ Nothing -> Nothing
     _ -> error "Trying to get a down message on an empty separator ! Should never occur !"

-- | Return the separator childrens of a node
nodeChildren :: Ord c => JTree c a -> c -> [Sep]
{-# INLINE nodeChildren #-}
nodeChildren t e = maybe [] id $! Map.lookup e (childrenMap t)

-- | Return the child of a separator
separatorChild :: Ord c => JTree c a -> Sep -> c 
{-# INLINE separatorChild #-}
separatorChild t e = fromJust $! Map.lookup e (separatorChildMap t)

-- | Check if a node is member of the tree
nodeIsMemberOfTree :: Ord c => c -> JTree c a -> Bool 
{-# INLINE nodeIsMemberOfTree #-}
nodeIsMemberOfTree c t = Map.member c (nodeValueMap t)

-- | Add a separator between two nodes.
-- The nodes MUST already be in the tree
addSeparator :: (Ord c) 
             => c -- ^ Origin node 
             -> c -- ^ Separator value
             -> c -- ^ Destination node 
             -> JTree c a -- ^ Current tree 
             -> JTree c a -- ^ Modified tree 
addSeparator node sepCluster dest t = 
    let newSep = (separatorCurrentKey t) + 1 
    in
    t { childrenMap = Map.insertWith' (++) node [newSep] (childrenMap t)
      , separatorChildMap = Map.insert newSep dest (separatorChildMap t)
      , separatorValueMap = Map.insert newSep EmptySeparator (separatorValueMap t)
      , separatorClusterMap = Map.insert newSep sepCluster (separatorClusterMap t)
      , leavesSet = Set.delete node (leavesSet t) 
      , parentMap = Map.insert dest newSep (parentMap t)
      , separatorParentMap = Map.insert newSep node (separatorParentMap t)
      , separatorCurrentKey = newSep
      }

-- | Add a new node
addNode :: (Ord c) 
        => c -- ^ Node
        -> Vertex
        -> [a] -- ^ Factor value 
        -> [a] -- ^ Evidence value
        -> JTree c a 
        -> JTree c a 
addNode node vertex factorValue evidenceValue t = 
   t { nodeValueMap = Map.insert node (NodeValue vertex factorValue evidenceValue) (nodeValueMap t)
     , leavesSet = Set.insert node (leavesSet t) 
     }

-- | Update the up message of a separator
updateUpMessage :: Ord c 
                => Maybe Sep -- ^ Separator node to update (if any : none for root node)
                -> a -- ^ New value
                -> JTree c a -- ^ Old tree
                -> JTree c a
updateUpMessage Nothing _ t = t
updateUpMessage (Just sep) newval t = 
    let newSepValue =  case separatorValue t sep of 
                         EmptySeparator -> SeparatorValue newval Nothing
                         SeparatorValue up down -> SeparatorValue newval down 
    in 
    t {separatorValueMap = Map.insert sep newSepValue (separatorValueMap t)}

-- | Update the down message of a separator
updateDownMessage :: Ord c 
                  => Sep -- ^ Separator node to update
                  -> a -- ^ New value
                  -> JTree c a -- ^ Old tree
                  -> JTree c a
updateDownMessage sep newval t = 
    let newSepValue = case separatorValue t sep of 
                        EmptySeparator -> error "Can't set a down message on an empty separator"
                        SeparatorValue up _ -> SeparatorValue up (Just newval)
    in 
    t {separatorValueMap = Map.insert sep newSepValue (separatorValueMap t)}

{-

Message passing algorithms
    
-}

-- | Functions used to generate new messages
class Message f c | f -> c where
    -- | Generate a new message from the received ones
    newMessage :: [f] -> NodeValue f -> c -> f 


-- | Check that a separator is initialized
separatorInitialized :: SeparatorValue a -> Bool
separatorInitialized EmptySeparator = False 
separatorInitialized _ = True

allSeparatorsHaveReceivedAMessage :: Ord c
                                  => JTree c a -- ^ Tree
                                  -> [Sep] -- ^ Separators
                                  -> Bool 
allSeparatorsHaveReceivedAMessage t seps = 
  all separatorInitialized . map (separatorValue t) $ seps

-- | Update the up separator by sending a message
-- But only if all the down separators have received a message
updateUpSeparator :: (Message a c, Ord c) 
                  => JTree c a 
                  -> c -- ^ Node generating the new upMessage
                  -> JTree c a 
updateUpSeparator t h  = 
    let seps = nodeChildren t h
    in
    case allSeparatorsHaveReceivedAMessage t seps of 
      False -> t 
      True -> let incomingMessages = map (upMessage t) seps
                  currentValue = nodeValue t h
                  destinationNode = nodeParent t h
              in 
              case destinationNode of 
                Nothing -> t -- When root
                Just p -> let  sepC = separatorCluster t p
                               generatedMessage = newMessage incomingMessages currentValue sepC
                          in 
                          updateUpMessage destinationNode generatedMessage t

-- | Update the down separator by sending a message
updateDownSeparator :: (Message a c, Ord c) 
                    => c -- ^ Node generating the message 
                    -> JTree c a 
                    -> Sep -- ^ Child receiving the message
                    -> JTree c a 
updateDownSeparator node t child  = 
    let incomingMessagesFromBelow = map (upMessage t) (nodeChildren t node \\ [child])
        messageFromAbove = downMessage t =<< (nodeParent t node)
        incomingMessages = maybe incomingMessagesFromBelow (\x -> x:incomingMessagesFromBelow) messageFromAbove
        currentValue = nodeValue t node
        childC = separatorCluster t child
        generatedMessage = newMessage incomingMessages currentValue childC
    in 
    updateDownMessage child generatedMessage t

unique :: Ord c => [c] -> [c]
{-# INLINE unique #-}
unique = Set.toList . Set.fromList

-- | Collect message taking into account that the tree depth may be different for different leaves.
collect :: (Ord c, Message a c) 
        => JTree c a 
        -> JTree c a
collect t = _collectNodes (leaves t) t

_collectSeparators :: (Ord c, Message a c) 
                   => [Sep]
                   -> JTree c a -- ^ Tree
                   -> JTree c a -- ^ Modified tree
_collectSeparators l t = _collectNodes (unique . map (separatorParent t) $ l) t

_collectNodes :: (Ord c, Message a c) 
              => [c]
              -> JTree c a -- ^ Tree
              -> JTree c a -- ^ Modified tree 
_collectNodes  [] t = t
_collectNodes  l t = 
    let newTree = foldl' updateUpSeparator t l
    in 
    _collectSeparators (mapMaybe (nodeParent t) l) newTree
    
distribute :: (Ord c, Message a c)
           => JTree c a 
           -> JTree c a
distribute t = _distributeNodes t (root t) 

_distributeSeparators :: (Ord c, Message a c)
                      => JTree c a 
                      -> Sep -- ^ Destination of the distribute
                      -> JTree c a 
_distributeSeparators t node = _distributeNodes t (separatorChild t node)

_distributeNodes :: (Ord c, Message a c)
                 => JTree c a 
                 -> c -- ^ Destination of the distribute
                 -> JTree c a 
_distributeNodes t node = 
    let children = nodeChildren t node
        newTree = foldl' (updateDownSeparator node) t $ children
    in
    foldl' _distributeSeparators newTree children

{-

Factors and evidence modifications

-}

-- | This class is used to check if evidence or a factor is relevant
-- for a cluster
class IsCluster c where 
  -- | Evidence contained in the cluster
  overlappingEvidence :: c -> [DVI] -> [DVI]
  -- | Cluser variables
  clusterVariables :: c -> [DV]
  -- | Intersection of two clusters
  mkSeparator :: c -> c -> c

instance IsCluster [DV] where 
  overlappingEvidence c e = filter (\x -> instantiationVariable x `elem` c) e
  clusterVariables = id
  mkSeparator = intersect

data Action s a = Skip !s 
                | ModifyAndStop !s !a
                | Modify !s !a
                | Stop !s

-- | Traverse a tree and modify it
traverseTree :: Ord c 
             => (s -> c -> NodeValue f -> Action s (NodeValue f)) -- ^ Modification function
             -> s -- ^ Current state
             -> JTree c f -- ^ Input tree
             -> (JTree c f,s)
traverseTree action state t = _traverseTreeNodes action (t,state) (root t)

_traverseTreeSeparators action (t,state) current = _traverseTreeNodes  action (t,state)  (separatorChild t current) 

_traverseTreeNodes action (t,state) current = 
  case action state current (nodeValue t current) of 
     Stop newState -> (t,newState)
     ModifyAndStop _ newValue -> (setNodeValue current newValue t, state) 
     Skip newState -> foldl' (_traverseTreeSeparators action) (t,newState) (nodeChildren t current)
     Modify newState newValue -> 
         let newTree = setNodeValue current newValue t 
         in 
         foldl' (_traverseTreeSeparators action) (newTree,newState) (nodeChildren newTree current)

mapWithCluster :: Ord c 
               => (c -> NodeValue f -> NodeValue f)
               -> JTree c f 
               -> JTree c f        
mapWithCluster f t = t {nodeValueMap = Map.mapWithKey f (nodeValueMap t)}

-- | Set the factors in the tree 
updateTreeValues :: (Factor f, IsCluster c, Ord c, Show c, Show f)
                 => (f -> NodeValue f -> NodeValue f) 
                 -> [f]
                 -> JTree c f 
                 -> JTree c f
updateTreeValues change factors t = 
  let allNodes = treeNodes t
      factorIncludedInCluster f c = all (`elem` clusterVariables c) (factorVariables f)
      coveringClusters f = filter (f `factorIncludedInCluster`) allNodes
      clusterSize a = product . map (fromIntegral . dimension) . clusterVariables $ a :: Integer
      addFactor t newFactor = 
        let minimumCluster = minimumBy (compare `on` clusterSize) (coveringClusters newFactor)
            clusterValue = nodeValue t minimumCluster
        in 
        setNodeValue minimumCluster (change newFactor clusterValue) t
  in 
  foldl' addFactor t factors

-- | Set the factors in the tree 
setFactors :: (Graph g, Factor f, IsCluster c, Ord c, Show c, Show f)
           => BayesianNetwork g f 
           -> JTree c f 
           -> JTree c f
setFactors g t = 
  let factors = allVertexValues g 
      changeFactor f (NodeValue v oldf e) = NodeValue v (f:oldf) e
  in 
  updateTreeValues  changeFactor factors t  

-- | Change evidence in the network
changeEvidence :: (IsCluster c, Ord c, Factor f, Message f c, Show c, Show f)
               => [DVI] -- ^ Evidence
               -> JTree c f 
               -> JTree c f 
changeEvidence e t =  
  let evidences = map factorFromInstantiation e
      changeEvidence newe (NodeValue v f olde) = NodeValue v f (newe:olde)
  in 
  distribute . 
  collect .
  updateTreeValues changeEvidence evidences .
  resetEvidences $
  t { separatorValueMap = Map.map (const EmptySeparator) (separatorValueMap t)}                

-- | Cluster of discrete variables.
-- Discrete variables instead of vertices are needed because the
-- factor are using 'DV' and we need to find
-- which factors must be contained in a given cluster.
newtype Cluster = Cluster (Set.Set DV) deriving(Eq,Ord)

instance IsCluster Cluster where 
  overlappingEvidence c = overlappingEvidence (fromCluster c)
  clusterVariables c = clusterVariables (fromCluster c)
  mkSeparator (Cluster a) (Cluster b) = Cluster (Set.intersection a b)

instance Show Cluster where 
  show (Cluster s) = show . Set.toList $ s

fromCluster (Cluster s) = Set.toList s 


instance (Factor f,IsBucketItem f) => Message f Cluster where 
  newMessage input (NodeValue _ f e) c = 
    let allFactors = f ++ e ++ input 
        variablesToKeep = fromCluster c 
        variablesToRemove = (nub (concatMap factorVariables allFactors)) \\ variablesToKeep
    in 
    marginal allFactors variablesToRemove variablesToKeep []


type JunctionTree f = JTree Cluster f

{-

Implement the show function to see the structure of the tree
(without the values)
    
-}

data NodeKind c = N !c | S !c

label True c a = c ++ "=" ++ show a 
label False c _ = c

-- | Convert the JTree into a tree of string
-- using the cluster.
toTree :: (Ord c, Show c, Show a) 
       => Bool -- ^ True if the data must be displayed
       -> JTree c a 
       -> Tree.Tree String
toTree d t = 
    let r = root t
        v = nodeValue t r
        nodec = nodeChildren t r
    in 
    Tree.Node (label d (show r) v) (_toTreeSeparators d t nodec)


_toTreeNodes :: (Ord c, Show c, Show a) 
             => Bool
             -> JTree c a 
             -> [c] 
             -> [Tree.Tree String]
_toTreeNodes _ _ [] = []
_toTreeNodes d t (h:l) = 
    let nodec = nodeChildren t h -- Node children are separators
        v = nodeValue t h
    in
    Tree.Node (label d (show h) v) (_toTreeSeparators d t nodec):_toTreeNodes d t l

_toTreeSeparators :: (Ord c, Show c, Show a) 
                  => Bool
                  -> JTree c a 
                  -> [Sep] 
                  -> [Tree.Tree String]
_toTreeSeparators _ _ [] = []    
_toTreeSeparators d t (h:l) = 
    let separatorc = [separatorChild t h] -- separator child is a node
        v = separatorValue t h
    in
    Tree.Node (label d ("<" ++ show (separatorCluster t h) ++ ">") v ) (_toTreeNodes d t separatorc):_toTreeSeparators d t l

instance (Ord c, Show c, Show a) => Show (JTree c a) where 
    show = Tree.drawTree . toTree False

displayTree b = Tree.drawTree . toTree b

-- | Display the tree values
displayTreeValues :: (Show f, Show c) => JTree c f -> IO ()
displayTreeValues t = 
  let allValues = treeValues t
      printAValue (c,NodeValue _ f e) = do 
        print c 
        putStrLn "FACTOR"
        print f 
        putStrLn "EVIDENCE"
        print e 
        putStrLn "------"

  in 
  mapM_ printAValue allValues


