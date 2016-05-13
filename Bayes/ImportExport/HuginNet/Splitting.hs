module Bayes.ImportExport.HuginNet.Splitting ( 
	  splitCPT
	, splitValues
	) where 

import Data.List.Split

splitCPT = split (dropBlanks . dropDelims $ oneOf "() |") 
splitValues = split (dropBlanks . dropDelims $ oneOf "() \n\t") 
