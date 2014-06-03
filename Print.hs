-- Print module
-- By G.W. Schwartz
--
-- Collection of functions for the printing of data (converting data
-- structures into strings for use with writing to output files).

module Print where

-- | Built-in
import Data.List
import Data.Maybe
import Data.Ord

-- | Local
import Types

-- Return the results of the filtration in string form for saving
-- to a file
printFasta :: Bool -> [FastaSequence] -> String
printFasta = body
  where
    body False          = intercalate "\n"
                        . map clipVsFasta
    body True           = intercalate "\n" . map cloneToString . cloneList
    clipVsFasta x
        | isJust . germline $ x = concat [ ">>Germline|"
                                         , fastaInfo x
                                         , "\n"
                                         , fromMaybe "" . germline $ x
                                         , "\n>"
                                         , fastaInfo x
                                         , "\n"
                                         , fastaSeq x ]
        | otherwise = seqToString x
    cloneToString (x:xs) = concat [ ">>Germline|"
                                  , fromMaybe "" . cloneID $ x
                                  , "\n"
                                  , fromMaybe "" . germline $ x
                                  , "\n"
                                  , seqToString x
                                  , "\n"
                                  , intercalate "\n" . map seqToString $ xs
                                  ]
    cloneList = groupBy (\x y -> cloneID x == cloneID y)
              . sortBy (comparing cloneID)
    seqToString x = ">" ++ fastaInfo x ++ "\n" ++ fastaSeq x
