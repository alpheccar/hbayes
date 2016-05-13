{- | Miscellaneous tools
	
-}
module Bayes.Tools (
	  nearlyEqual
  , withTempFile
  , minBoundForEnum 
  , maxBoundForEnum 
  , intValue
	) where 

import System.IO(openTempFile,hClose)
import System.Directory(getTemporaryDirectory,removeFile)
import System.FilePath

-- | Floating point number comparisons which should take into account
-- all the subtleties of that kind of comparison
nearlyEqual :: Double -> Double -> Bool
nearlyEqual a b = 
    let absA = abs a 
        absB = abs b 
        diff = abs (a-b)
        epsilon = 2e-5
    in
    case (a,b) of 
        (x,y) | x == y -> True -- handle infinities
              | x*y == 0 -> diff < (epsilon * epsilon)
              | otherwise -> diff / (absA + absB) < epsilon


-- | Execute an action with a temporary file. The file is deleted after.
-- The action must close the file.
-- (would be better to use handle to force the closing but it is used with action which are
-- using a filepath)
withTempFile :: (FilePath -> IO a) -> IO a 
withTempFile action = do 
  tempDir <- getTemporaryDirectory 
  (filePath,fileHandle) <- openTempFile tempDir "bayestest" 
  hClose fileHandle 
  result <- action filePath 
  removeFile (tempDir </> filePath)
  return result 

minBoundForEnum :: Bounded a => a -> a
minBoundForEnum _ = minBound

maxBoundForEnum :: Bounded a => a -> a
maxBoundForEnum _ = maxBound

intValue :: Enum a => a -> Int
intValue = fromEnum