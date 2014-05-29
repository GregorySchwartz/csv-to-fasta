-- Parse module
-- By Gregory W. Schwartz

-- Collects all functions pertaining to the parsing of strings to data
-- structures

module Parse where

-- | Built-in
import Data.List
import Data.Maybe

-- | Cabal
import qualified Data.List.Split as Split

-- | Local
import Types

-- | Parse a string to a list of fasta sequences
parseCSV :: Bool
         -> [String]
         -> [Int]
         -> String
         -> Int
         -> String
         -> String
         -> [FastaSequence]
parseCSV noHeader headers headerCols seqs seqCol sep contents = map lineToSeq
                                                              . zip [1..]
                                                              . filterShortLines
                                                              . body noHeader
                                                              $ contents
  where
    lineToSeq = convertToFastaSeq headerList
              . splitLine
    convertToFastaSeq [-1] (x, xs) = FastaSequence { fastaInfo = show x
                                                   , fastaSeq  = xs !! mainSeq
                                                   }
    convertToFastaSeq _ (_, xs)    = FastaSequence { fastaInfo = intercalate "|"
                                                               . map
                                                                 (getHeader xs)
                                                               $ headerList
                                                   , fastaSeq  = xs !! mainSeq
                                                   }
    getHeader xs x      = xs !! x
    splitLine (x, xs)   = (x, Split.splitOn sep xs)
    filterShortLines    = filter ( \x -> length (Split.splitOn sep x)
                                      >= max (maximum headerList) mainSeq )
    body True           = lines
    body False          = tail . lines
    header              = Split.splitOn sep . head . lines $ contents
    headerList
        | null headers  = map (flip (-) 1) headerCols
        | otherwise     = mapMaybe (`elemIndex` header) headers
    mainSeq
        | null seqs     = seqCol - 1
        | otherwise     = fromJust . elemIndex seqs $ header


-- | Counts the number of times a substring appears in a string
count :: (Eq a) => a -> [a] -> Int
count x = foldr (\y acc -> if y == x then acc + 1 else acc) 0
