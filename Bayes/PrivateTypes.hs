{- | Private types for Bayes and Factors.

Those type are not exported

-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bayes.PrivateTypes( 
 -- * Classes and types
   BayesianDiscreteVariable(..)
 , BayesianVariable(..)
 , Set(..)
 , BayesianNetwork(..)
 -- * Variables
 , DV(..)
 , DVSet(..)
 , DVISet(..)
 , TDV
 , tdv
 , tdvi
 , CV(..)
 , CVI(..)
 -- * Instantiations
 , Instantiable(..)
 , InstantiationValue(..)
 , DVI(..)
 , setDVValue
 , instantiationVariable
 , fromDVSet
 -- * Vertices, Graph
 , Vertex(..)
 , Edge(..)
 , SimpleGraph(..)
 , DE(..)
 , UE(..)
 -- * Misc
 , getMinBound
 -- * Indices 
 , MultiIndex(..)
 , forAllInstantiations 
 , indicesForDomain
 , instantiationDetails
 , instantiation
 , allInstantiationsForOneVariable
 -- * For sampling 
 , Sample(..)
 , DistributionSupport(..)
 , DistributionF(..)
 -- * Tests 
 , instantiationProp
 ) where


import qualified Data.List as L 
import qualified Data.Vector.Unboxed as V
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import System.Random(Random)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Control.Monad.Reader 

-- | Bayesian network. g must be a directed graph and f a factor
type BayesianNetwork g f = g () f

{-
For sampling
-}



-- | A sample (graph of instantiations)
type Sample g a = BayesianNetwork g a

-- | Support of the distributiob p(x | abc ...) for x depending on the value of a,b,c ...
data DistributionSupport a = BoundedSupport !a 
                           | Unbounded !Double -- ^ Standard deviation
                           deriving(Eq)

-- | Distribution function with a possible bound for the function target
-- The bound may depend on the current sample (because for unbounded we need a variance to be able
-- to cover enough of the support)
data DistributionF g bound inst = Distri !(Reader (Sample g inst) (DistributionSupport bound)) !(inst -> Reader (Sample g inst) Double)



{-
	Set
-}

-- | A Set of variables used in a factor. s is the set and a the variable
class Set s where
    -- | Empty set
    emptySet :: s a
    -- | Union of two sets
    union :: Eq a => s a -> s a -> s a
    -- | Intersection of two sets
    intersection :: Eq a => s a -> s a -> s a
    -- | Difference of two sets
    difference :: Eq a => s a -> s a -> s a
    -- | Check if the set is empty
    isEmpty :: s a -> Bool
    -- | Check if an element is member of the set
    isElem :: Eq a => a -> s a -> Bool
    -- | Add an element to the set
    addElem :: Eq a => a -> s a -> s a
    -- | Number of elements in the set
    nbElements :: s a -> Int

    -- | Check if a set is subset of another one
    subset :: Eq a => s a -> s a -> Bool

    -- | Check set equality
    equal :: Eq a => s a -> s a -> Bool
    equal sa sb = (sa `subset` sb) && (sb `subset` sa)

instance Set [] where
    emptySet = []
    union = L.union
    intersection = L.intersect
    difference a b = a L.\\ b
    isEmpty [] = True 
    isEmpty _ = False
    isElem = L.elem 
    addElem a l = if a `elem` l then l else a:l
    nbElements = length
    subset sa sb = all (`elem` sb) sa

{-

Misc

-}
-- | Vertex type used to identify a vertex in a graph
newtype Vertex = Vertex {vertexId :: Int} deriving(Eq,Ord)

-- | Edge type used to identify and edge in a graph
data Edge = Edge !Vertex !Vertex deriving(Eq,Ord,Show)

-- | Implementtaion of a SimpleGraph
data SimpleGraph local edgedata vertexdata = SP {
 -- | Mapping of edge to edge data
   edgeMap :: !(M.Map Edge edgedata) 
 -- ^ Mapping of vertex number to vertex neighborhood and vertex data
 , vertexMap :: !(IM.IntMap (local, vertexdata))
 -- ^ Vertex names. Used only to generate the graphviz representation. Names are useless for the algorithms
 -- and I don't want them to appear in the vertex values which should only be factor. Otherwise, the algorithms
 -- are less elegant since I have to extract the factors from the values
 , nameMap :: !(IM.IntMap String)
 } 

-- | Neighborhood structure for directed or undirected edges
-- | Directed edges
data DE = DE ![Edge] ![Edge] deriving(Eq,Show)

-- | Undirected edges
data UE = UE ![Edge] deriving(Eq,Show)

instance Show Vertex where 
    show (Vertex v) = "v" ++ show v

-- | A Bayesian Variable is a variable part of Bayesian network and so which knows
-- its position : the vertex.
class BayesianVariable v where 
  vertex :: v -> Vertex 

-- | A discrete variable has a number of levels which is required to size the factors
class BayesianVariable v => BayesianDiscreteVariable v where
    dimension :: v -> Int 
    dv :: v -> DV

-- | Get the minimum bound for a type
getMinBound :: Bounded a => a -> a 
getMinBound _ = minBound

instance BayesianVariable Vertex where 
  vertex v = v 
  
{-

Variables
	
-}

-- | A continuous variable
data CV = CV !Vertex deriving(Eq,Ord)

instance Show CV where 
  show (CV v) = show v

instance BayesianVariable CV where 
  vertex (CV v) = v 

-- | A continuous variable instantiation 
data CVI = CVI !CV !Double deriving(Eq,Ord,Show)

instance BayesianVariable CVI where 
  vertex (CVI v _) = vertex v

-- | A discrete variable
data DV = DV !Vertex !Int deriving(Eq,Ord)

-- | A set of discrete variables
-- The tag is used to check that an index is used with the right set of DV
newtype DVSet s = DVSet [DV] deriving(Eq,Show)

-- | Remove the type tag when not needed
fromDVSet :: DVSet s -> [DV]
fromDVSet (DVSet l) = l

instance Show DV where
    show (DV v d) = show v ++ "(" ++ show d ++ ")"

instance BayesianVariable DV where 
    vertex (DV v _) = v

instance BayesianDiscreteVariable DV where
    dimension (DV _ d) = d
    dv = id

-- | A typed discrete variable
data TDV s = TDV !Vertex !Int deriving(Eq,Ord)

instance Show (TDV s) where
    show (TDV v d) = show v

instance BayesianVariable (TDV s) where 
    vertex (TDV v _) = v

instance BayesianDiscreteVariable (TDV s) where
    dimension (TDV _ d) = d
    dv (TDV v nb) = DV v nb

-- | Typed discrete variable
tdv :: DV -> TDV s 
tdv (DV v nb) = TDV v nb

-- | Typed instantiation 
tdvi :: Enum s => DVI -> (TDV s,s)
tdvi (DVI dv value) = (tdv dv, toEnum value)
{-

Index

-}


newtype MultiIndex s = MultiIndex (V.Vector Int) deriving(Eq,Show)

-- | Get the instantiations for a given multindex
instantiation :: DVSet s -> MultiIndex s -> [DVI]
instantiation (DVSet l) (MultiIndex v) = zipWith setDVValue l (V.toList v)

-- | Generate all the indices for a set of variables
indicesForDomain :: DVSet s -> [MultiIndex s]
{-# INLINE indicesForDomain #-}
indicesForDomain (DVSet l) = map (MultiIndex . V.fromList) $ (mapM indicesForOneDomain l)
 where 
 	indicesForOneDomain (DV _ d) = [0..d-1]

allInstantiationsForOneVariable :: DV -> [DVI]
allInstantiationsForOneVariable v@(DV _ d) = map (setDVValue v) [0..d-1]

-- | Generate all instantiations of variables
-- The DVInt can be in any order so the tag s is not used
forAllInstantiations :: DVSet s -> [[DVI]]
forAllInstantiations (DVSet l) = mapM allInstantiationsForOneVariable l
 



{- 

Instantiations

-}
-- | Discrete Variable instantiation. A variable and its value
data DVI = DVI DV !Int deriving(Eq)

instance Show (DVI) where 
   show (DVI (DV v _) i) = show v ++ "=" ++ show i

   -- | A set of variable instantiations
type DVISet = [DVI]

class InstantiationValue i v | i -> v where 
  instantiationValue :: i -> v 
  toDouble :: i -> Double

instance InstantiationValue DVI Int where 
  -- | Extract value of the instantiation
  instantiationValue (DVI _ v) = v
  toDouble (DVI _ v) = fromIntegral v

instance InstantiationValue CVI Double where 
  -- | Extract value of the instantiation
  instantiationValue (CVI _ v) = v
  toDouble (CVI _ v) = v

class Instantiable d v r | d -> r where 
  -- | Create a variable instantiation using values from
  -- an enumeration
  (=:) :: d -> v -> r 

instance Instantiable CV Double CVI where 
  (=:) c x = CVI c x

instance (Bounded b, Enum b) => Instantiable DV b DVI where
  (=:) a b = setDVValue a (fromEnum b - fromEnum (getMinBound b))

instance (Bounded b, Enum b) => Instantiable (TDV b) b DVI where
  (=:) (TDV v nb) b = setDVValue (DV v nb) (fromEnum b - fromEnum (getMinBound b))


-- | Create a discrete variable instantiation for a given discrete variable
setDVValue :: DV -> Int -> DVI
setDVValue v a = DVI v a


instance BayesianVariable DVI where 
    vertex (DVI dv _) = vertex dv

instance BayesianDiscreteVariable DVI where
    dimension (DVI v _) = dimension v
    dv = instantiationVariable

-- | Get the variables and their values with a type constraint
instantiationDetails :: [DVI] -> (DVSet s, MultiIndex s)
instantiationDetails l = (DVSet $ map instantiationVariable l, MultiIndex . V.fromList . map (instantiationValue) $ l)



-- | Discrete variable from the instantiation
instantiationVariable (DVI dv _) = dv


{-

QuickCheck

-}

{-

CPT can't have same same vertex values but with different sizes.
But, arbitrary CPT generation will general several vertex with same vertex id
and different vertex size.

So, we introduce a function mapping a vertex ID to a vertex size. So, vertex size are hard coded

-}

quickCheckVertexSize :: Int -> Int
quickCheckVertexSize 0 = 2
quickCheckVertexSize 1 = 2
quickCheckVertexSize 2 = 2
quickCheckVertexSize _ = 2

-- | Generate a random value until this value is not already present in the list
whileIn :: (Arbitrary a, Eq a) => [a] -> Gen a -> Gen a
whileIn l m = do 
    newVal <- m 
    if newVal `elem` l 
        then
            whileIn l m 
        else 
            return newVal

-- | Generate a random vector of n elements without replacement (no duplicate)
-- May loop if the range is too small !
generateWithoutReplacement :: (Random a, Arbitrary a, Eq a)  
                           => Int -- ^ Vector size
                           -> (a,a) -- ^ Bounds
                           -> Gen [a]
generateWithoutReplacement n b | n == 1 = generateSingle b 
                               | n > 1 = generateMultiple n b 
                               | otherwise = return []
 where
   generateSingle b = do 
       r <- choose b
       return [r]
   generateMultiple n b = do 
       l <- generateWithoutReplacement (n-1) b
       newelem <- whileIn l $ choose b
       return (newelem:l)

-- | Check that we can recover an instantiation from a MultiIndex
instantiationProp :: DVSet s -> Bool 
instantiationProp dvl = 
    let dvs = DVSet (fromDVSet dvl) 
    in 
    forAllInstantiations dvs == map (instantiation dvs) (indicesForDomain dvs) 


instance Arbitrary (DVSet s) where 
    arbitrary =  do
        nbVertex <- choose (1,4) :: Gen Int
        vertexNumbers <- generateWithoutReplacement nbVertex (0,50)
        let dimensions = map (\i -> (DV (Vertex i)  (quickCheckVertexSize i))) vertexNumbers
        return (DVSet dimensions)

