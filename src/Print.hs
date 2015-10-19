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
printFasta :: Bool -> Bool -> [FastaSequence] -> T.Text
printFasta cloneFlag noSortCloneFlag = body
  where
    body = if cloneFlag
            then T.unlines . map cloneToString . cloneList
            else T.unlines . map clipVsFasta
    clipVsFasta x
        | isJust . germline $ x =
            T.intercalate "\n" [ ">>Germline|" `T.append` fastaInfo x
                               , fromMaybe "" . germline $ x
                               , ">" `T.append` fastaInfo x
                               , fastaSeq x ]
        | otherwise = seqToString x
    cloneToString (x:xs) =
        T.intercalate "\n" [ ">>Germline|" `T.append` (fromMaybe "" (cloneID x))
                           , fromMaybe "" . germline $ x
                           , seqToString x
                           , T.intercalate "\n" . map seqToString $ xs
                           ]
    cloneList = groupBy (\x y -> cloneID x == cloneID y)
              . sortClones noSortCloneFlag
    sortClones True  = id
    sortClones False = sortBy (comparing cloneID)
    seqToString x = T.concat [">", fastaInfo x, "\n", fastaSeq x]
