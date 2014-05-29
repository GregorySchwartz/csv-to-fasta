-- Print module
-- By G.W. Schwartz
--
-- Collection of functions for the printing of data (converting data
-- structures into strings for use with writing to output files).

module Print where

-- | Built-in
import Data.List

-- | Local
import Types

-- Return the results of the filtration in string form for saving
-- to a file
printFasta :: [FastaSequence] -> String
printFasta = body
  where
    body                = intercalate "\n"
                        . map (\x -> ">" ++ fastaInfo x ++ "\n" ++ fastaSeq x)
