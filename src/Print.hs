-- Print module
-- By G.W. Schwartz
--
-- Collection of functions for the printing of data (converting data
-- structures into strings for use with writing to output files).

{-# LANGUAGE OverloadedStrings #-}

module Print where

-- | Built-in
import Data.List
import Data.Maybe
import Data.Ord

-- | Cabla
import qualified Data.Text.Lazy as T

-- | Local
import Types

-- Return the results of the filtration in string form for saving
-- to a file
printFasta :: Bool -> [FastaSequence] -> T.Text
printFasta = body
  where
    body False          = T.unlines
                        . map clipVsFasta
    body True           = T.unlines . map cloneToString . cloneList
    clipVsFasta x
        | isJust . germline $ x = T.concat [ ">>Germline|"
                                           , fastaInfo x
                                           , "\n"
                                           , fromMaybe "" . germline $ x
                                           , "\n>"
                                           , fastaInfo x
                                           , "\n"
                                           , fastaSeq x ]
        | otherwise = seqToString x
    cloneToString (x:xs) = T.concat [ ">>Germline|"
                                    , fromMaybe "" . cloneID $ x
                                    , "\n"
                                    , fromMaybe "" . germline $ x
                                    , "\n"
                                    , seqToString x
                                    , "\n"
                                    , T.intercalate "\n" . map seqToString $ xs
                                    ]
    cloneList = groupBy (\x y -> cloneID x == cloneID y)
              . sortBy (comparing cloneID)
    seqToString x = T.concat [">", fastaInfo x, "\n", fastaSeq x]
