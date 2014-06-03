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
         -> Bool
         -> Bool
         -> [String]
         -> [Int]
         -> String
         -> Int
         -> String
         -> Int
         -> String
         -> Int
         -> String
         -> String
         -> [FastaSequence]
parseCSV noHeader
         includeGermline
         includeClone
         headers
         headerCols
         seqs
         seqCol
         germ
         germCol
         clone
         cloneCol
         sep
         contents = map lineToSeq
                  . zip [1..]
                  . map (Split.splitOn sep)
                  . filterShortLines
                  . body noHeader
                  $ contents
  where
    lineToSeq = convertToFastaSeq headerList
    convertToFastaSeq [-1] (x, xs) = FastaSequence { fastaInfo = show x
                                                   , fastaSeq  = xs !! mainSeq
                                                   , germline  = getGerm
                                                                 includeGermline
                                                                 xs
                                                   , cloneID   = getClone
                                                                 includeClone
                                                                 xs
                                                   }
    convertToFastaSeq _ (_, xs)    = FastaSequence { fastaInfo = intercalate "|"
                                                               . map
                                                                 (getHeader xs)
                                                               $ headerList
                                                   , fastaSeq  = xs !! mainSeq
                                                   , germline  = getGerm
                                                                 includeGermline
                                                                 xs
                                                   , cloneID   = getClone
                                                                 includeClone
                                                                 xs
                                                   }
    getHeader xs x      = xs !! x
    filterShortLines    = filter ( \x -> length (Split.splitOn sep x)
                                      >= maxField )
    maxField            = maximum (headerList ++ [mainSeq, mainGerm, mainClone])
    body True           = lines
    body False          = tail . lines
    header              = Split.splitOn sep . head . lines $ contents
    getGerm True xs     = Just (xs !! mainGerm)
    getGerm False _     = Nothing
    getClone True xs    = Just (xs !! mainClone)
    getClone False _    = Nothing
    headerList
        | null headers  = map (flip (-) 1) headerCols
        | otherwise     = mapMaybe (`elemIndex` header) headers
    mainSeq
        | null seqs     = seqCol - 1
        | otherwise     = fromJust . elemIndex seqs $ header
    mainGerm
        | null germ     = germCol - 1
        | otherwise     = fromJust . elemIndex germ $ header
    mainClone
        | null clone     = cloneCol - 1
        | otherwise     = fromJust . elemIndex clone $ header


-- | Counts the number of times a substring appears in a string
count :: (Eq a) => a -> [a] -> Int
count x = foldl' (\acc y -> if y == x then acc + 1 else acc) 0
