-- Parse module
-- By Gregory W. Schwartz

-- Collects all functions pertaining to the parsing of strings to data
-- structures

{-# LANGUAGE OverloadedStrings #-}

module Parse where

-- | Built-in
import Data.List
import Data.Maybe

-- | Cabal
import qualified Data.Text.Lazy as T

-- | Local
import Types

-- | Parse a string to a list of fasta sequences
parseCSV :: Bool
         -> Bool
         -> Bool
         -> [T.Text]
         -> [Int]
         -> T.Text
         -> Int
         -> T.Text
         -> Int
         -> T.Text
         -> Int
         -> T.Text
         -> T.Text
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
                  . zip ([1..] :: [Int])
                  . map (T.splitOn sep)
                  . filterShortLines
                  . body noHeader
                  $ contents
  where
    lineToSeq = convertToFastaSeq headerList
    convertToFastaSeq [-1] (x, xs) = FastaSequence { fastaInfo = showText x
                                                   , fastaSeq  = xs !! mainSeq
                                                   , germline  = getGerm
                                                                 includeGermline
                                                                 xs
                                                   , cloneID   = getClone
                                                                 includeClone
                                                                 xs
                                                   }
    convertToFastaSeq _ (_, xs)    = FastaSequence { fastaInfo = T.intercalate
                                                                 "|"
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
    filterShortLines    = filter ( \x -> length (T.splitOn sep x)
                                      >= maxField )
    maxField            = maximum (headerList ++ [mainSeq, mainGerm, mainClone])
    body True           = T.lines
    body False          = tail . T.lines
    header              = T.splitOn sep . head . T.lines $ contents
    getGerm True xs     = Just (xs !! mainGerm)
    getGerm False _     = Nothing
    getClone True xs    = Just (xs !! mainClone)
    getClone False _    = Nothing
    headerList
        | null headers  = map (flip (-) 1) headerCols
        | otherwise     = mapMaybe (`elemIndex` header) headers
    mainSeq
        | T.null seqs   = seqCol - 1
        | otherwise     = fromMaybe (error "Sequence column not found")
                        . elemIndex seqs
                        $ header
    mainGerm
        | T.null germ   = germCol - 1
        | otherwise     = fromMaybe (error "Germline column not found")
                        . elemIndex germ
                        $ header
    mainClone
        | T.null clone  = cloneCol - 1
        | otherwise     = fromMaybe (error "Clone column not found")
                        . elemIndex clone
                        $ header

-- | Counts the number of times a substring appears in a string
count :: (Eq a) => a -> [a] -> Int
count x = foldl' (\acc y -> if y == x then acc + 1 else acc) 0
