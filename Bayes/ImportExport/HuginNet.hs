-- | Parser for a subset of the Hugin Net language
module Bayes.ImportExport.HuginNet( 
    importBayesianGraph
    ) where 

import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char 
import Text.ParserCombinators.Parsec.Combinator 

import Data.Maybe(mapMaybe,fromJust)
import Bayes.ImportExport.HuginNet.Splitting 
import qualified Data.Map as Map
import Bayes.Factor
import Bayes
import Bayes.PrivateTypes
import Bayes.Factor.CPT(changeVariableOrder)
import Bayes.BayesianNetwork

--import Debug.Trace 

--debug a = trace (show a) a

data Section = Net 
             | Node String [String] Int
             | Potential [String] [String]
             deriving(Eq,Show)

name :: Parser String 
name = many1 (alphaNum <|> oneOf "_-")

sectionContent :: Parser ()
sectionContent = do 
    string "{"
    newline
    many1 (noneOf "}")
    string "}" 
    optional newline
    return ()

net :: Parser Section
net = do 
    string "net"
    newline
    sectionContent
    return Net

levelName = do 
    char '"'
    s <- many1 (noneOf "\"")
    char '"'
    return s 

-- | Node states
state :: Parser [String]
state = do 
    spaces
    string "states"
    spaces
    char '='
    spaces 
    char '('
    spaces
    levels <- sepEndBy1 levelName (many1 space)
    char ')'
    spaces
    char ';'
    spaces
    optional newline
    return levels

factorValues :: Parser String
factorValues = do 
    spaces 
    string "data"
    spaces
    char '='
    spaces
    r <- many1 (noneOf ";")
    spaces 
    optional newline 
    return r

unknownCommand = do 
    manyTill (noneOf "}") newline 
    return Nothing

recognizedCommand :: Parser a -> Parser (Maybe a)
recognizedCommand c =  choice [try c >>= return . Just, unknownCommand]

node :: Parser Section
node = do 
    string "node"
    spaces
    n <- name
    newline
    string "{"
    newline
    l <- many (recognizedCommand state)
    string "}" 
    optional newline
    let r = concat . mapMaybe id $ l
    return $ Node n r (length r)

potential :: Parser Section
potential = do 
    string "potential"
    spaces 
    conditions <- manyTill anyChar newline
    string "{"
    newline
    l <- many (recognizedCommand factorValues)
    string "}" 
    optional newline
    let r = concat . mapMaybe id $ l
    return $ Potential (splitCPT conditions) (splitValues r)

section :: Parser Section
section = choice [try net,try node,try potential]

comment = do 
    string "%%"
    manyTill anyChar newline
    return () 

manyEmpty = skipMany (space <|> newline)

netParser :: Parser [Section]
netParser = do
    many comment
    manyEmpty
    sepEndBy1 section manyEmpty

addVertexName (Node s _ d) (c,m) = (c+1,Map.insert s (DV (Vertex c) d) m)
addVertexName _ (c,m) = (c,m)

addSection m (Node _ _ _) = return ()

addSection m (Net) = return ()
addSection m (Potential conditions values) = do 
    let dvs = fromJust . mapM (flip Map.lookup m) $ conditions
        dst = head dvs 
        conds = tail dvs
        oldOrder = conds ++ [dst]
        dvalues = map read values :: [Double]
        newvalues = changeVariableOrder (DVSet oldOrder) (DVSet dvs) dvalues
    cpt dst conds ~~ newvalues
    return ()

addVariables (Node s _ d) = do 
    v <- variableWithSize s d
    return $ Just (s,v)

addVariables _ = return Nothing

-- | Import a bayesian network form a Hugin file.
-- Only a subset of the file format is supported.
-- You may have to convert the line endings to be able to parse a file
-- When it is succeeding, it is returing a bayesian network monad and
-- a mapping from node names to discrete variables.
importBayesianGraph :: Factor f 
                    => String 
                    -> IO (Maybe (BNMonad DirectedSG f (Map.Map String DV)))
importBayesianGraph s = do 
    r <- readBayesianNetwork s 
    case r of 
        Nothing -> return Nothing 
        Just s -> return . Just $ createBayesianGraph s

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f l = mapM f l >>= return . mapMaybe id

createBayesianGraph :: Factor f => [Section] ->  BNMonad DirectedSG f (Map.Map String DV)
createBayesianGraph s = do 
    vars <- mapMaybeM addVariables s
    let m = Map.fromList vars
    mapM_ (addSection m) s
    return m

-- | Horrible way to remove the comments
filterComment :: Bool -> String -> String
filterComment False ('%':l) = filterComment True l
filterComment False (a:l) = a:filterComment False l 
filterComment False [] = []
filterComment True ('\n':l) = '\n':filterComment False l 
filterComment True (a:l) = filterComment True l 
filterComment True [] = []

readBayesianNetwork s = do 
    f <- readFile s
    let result = runParser netParser () s (filterComment False f)
    case result of 
        Left err -> do 
            print err 
            return Nothing
        Right a -> return (Just a)