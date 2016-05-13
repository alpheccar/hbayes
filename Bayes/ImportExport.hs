{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- | Import / export Bayesian networks and junction tress

-}
module Bayes.ImportExport (
    -- * Networks
      writeNetworkToFile
    , readNetworkFromFile
    -- * Junction Tree 
    , writeVariableMapAndJunctionTreeToFile
    , readVariableMapAndJunctionTreeToFile
    ) where 

import Data.Binary
import Bayes
import Bayes.PrivateTypes(Vertex(..),Edge(..), SimpleGraph(..),DE(..), UE(..),DV(..))
import Bayes.Factor.PrivateCPT(CPT(..), MAXCPT(..),PrivateCPT(..)) 
import System.FilePath
import qualified Data.Vector as NV
import qualified Data.Vector.Unboxed as V
import Bayes.FactorElimination.JTree(SeparatorValue(..),NodeValue(..),JTree(..),Cluster(..),JunctionTree(..))
import qualified Data.Map as M 

-- | Write a bayesian network to file
writeNetworkToFile :: FilePath -- ^ File path
                   -> SBN CPT -- ^ Bayesian network
                   -> IO () 
writeNetworkToFile f n = encodeFile f n 

-- | Read bayesian network from file 
readNetworkFromFile :: FilePath 
                    -> IO (SBN CPT)
readNetworkFromFile = decodeFile

-- | Write a junction tree and the variable map to a file 
writeVariableMapAndJunctionTreeToFile :: FilePath
                                      -> (M.Map String Vertex)
                                      -> JunctionTree CPT 
                                      -> IO ()
writeVariableMapAndJunctionTreeToFile f vm jt = encodeFile f (vm,jt)

-- | Read variable map and junction tree from file
readVariableMapAndJunctionTreeToFile :: FilePath 
                                     -> IO (M.Map String Vertex, JunctionTree CPT)
readVariableMapAndJunctionTreeToFile f = decodeFile f

instance Binary Cluster where 
    put (Cluster s) = put s 
    get = get >>= return . Cluster

instance (Ord c, Binary c, Binary f) => Binary (JTree c f) where 
    put (JTree r ls cm pm spm scm nvm svm sck sclm) = do 
        put r 
        put ls 
        put cm 
        put pm 
        put spm 
        put scm 
        put nvm 
        put svm 
        put sck 
        put sclm 
    get = do 
        r <- get
        ls <- get 
        cm <- get 
        pm <- get 
        spm <- get 
        scm <- get 
        nvm <- get 
        svm <- get 
        sck <- get 
        sclm <- get
        return $ JTree r ls cm pm spm scm nvm svm sck sclm

instance Binary a => Binary (NodeValue a) where 
    put (NodeValue v f e) = do 
        put v 
        put f 
        put e 
    get = do 
        v <- get 
        f <- get 
        e <- get
        return $ NodeValue v f e

instance Binary a => Binary (SeparatorValue a) where
    put (EmptySeparator) = do 
        putWord8 0 
    put (SeparatorValue a b) = do 
        putWord8 1 
        put a 
        put b 
    get = do 
        tag <- getWord8 
        case tag of 
            0 -> return EmptySeparator
            _ -> do 
                a <- get 
                b <- get 
                return $ SeparatorValue a b 

instance Binary (V.Vector Double) where 
    put = put . V.toList 
    get = get >>= return . V.fromList 

instance Binary (NV.Vector Double) where 
    put = put . NV.toList 
    get = get >>= return . NV.fromList 

instance Binary DV where 
    put (DV v i) = do 
        put v 
        put i 
    get = do 
        v <- get 
        i <- get 
        return $ DV v i 

instance Binary (v Double) => Binary (PrivateCPT v Double) where 
    put (Table d m v) = do 
        putWord8 0 
        put d 
        put m 
        put v 
    put (Scalar v) = do 
        putWord8 1 
        put v 
    get = do 
        tag <- getWord8 
        case tag of 
            0 -> do 
                d <- get 
                m <- get 
                v <- get 
                return $ Table d m v 
            _ -> get >>= return . Scalar 

instance Binary Vertex where 
  put (Vertex v) = put v 
  get = get >>= return . Vertex 

instance Binary Edge where 
    put (Edge va vb) = do 
        put va 
        put vb 
    get = do 
        va <- get
        vb <- get 
        return $ Edge va vb

instance (Binary l, Binary e, Binary v) => Binary (SimpleGraph l e v) where 
    put (SP e v n) = do 
        put e 
        put v 
        put n 
    get = do 
        e <- get 
        v <- get 
        n <- get 
        return $ SP e v n

instance Binary DE where 
    put (DE a b) = do 
        put a 
        put b 
    get = do 
        a <- get 
        b <- get 
        return $ DE a b 

instance Binary UE where 
    put (UE a) = put a 
    get = get >>= return . UE