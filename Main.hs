import Bayes.FactorElimination
import Data.Maybe(fromJust)
import qualified Data.Map as Map
import Bayes
import Bayes.Factor
import Bayes.ImportExport.HuginNet
import System.Directory(getHomeDirectory)
import System.FilePath((</>))
import Bayes.Examples.Tutorial
import qualified Data.Set as Set

genericExample :: String -> IO (Map.Map String DV,SBN CPT)
genericExample s = do 
    r <- importBayesianGraph s
    return (runBN $ fromJust r)

exampleDiabete = do 
    h <- getHomeDirectory
    genericExample $ h </> "Dropbox/bayes_examples/Diabetes.hugin"


testa = do
  (varmap,g) <- exampleDiabete
  let theMoralGraph = moralGraph g
      cmp = nodeComparisonForTriangulation
      (clusters,_) = triangulate (cmp theMoralGraph) theMoralGraph
      g'' = createClusterGraph clusters :: UndirectedSG Int VertexCluster
      t = minimumSpanningTree g''
      factorSet = Set.fromList (allVertices g) -- Tracking of factors which have not yet been put in the junction tree
      -- A vertex is linked with a factor so vertex is used as the identifier
      (newTree, _) = setFactors g t factorSet
  print $ newTree

main = do 
	miscDiabete
	--inferencesOnStandardNetwork