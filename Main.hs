-- csv-to-fasta
-- By Gregory W. Schwartz

-- Takes a csv file and return a fasta file where each sequence was from
-- a column in the csv file

-- Cabal
import Options.Applicative

-- Local
import Types
import Parse
import Print

-- Command line arguments
data Options = Options { inputHeaders    :: String
                       , inputHeaderCols :: String
                       , inputSeqs       :: String
                       , inputSeqsCol    :: Int
                       , inputGerm       :: String
                       , inputGermCol    :: Int
                       , inputClone      :: String
                       , inputCloneCol   :: Int
                       , inputLabel      :: String
                       , inputSep        :: String
                       , noHeader        :: Bool
                       , includeGermline :: Bool
                       , includeClone    :: Bool
                       , input           :: String
                       , output          :: String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> strOption
          ( long "headers"
         <> short 'n'
         <> metavar "STRING"
         <> value ""
         <> help "The column names headers separated by a space.\
                 \ Appears in the header in the order given. Has preference\
                 \ over header-cols" )
      <*> strOption
          ( long "header-cols"
         <> short 'N'
         <> metavar "INT"
         <> value "-1"
         <> help "The column numbers for the header separated by a space.\
                 \ Appears in the header in the order given" )
      <*> strOption
          ( long "seqs"
         <> short 's'
         <> metavar "STRING"
         <> value ""
         <> help "The column name for the sequences. Has preference over\
                 \ seqs-col" )
      <*> option
          ( long "seqs-col"
         <> short 'S'
         <> metavar "INT"
         <> value 1
         <> help "The column number for the sequences" )
      <*> strOption
          ( long "germline"
         <> short 'g'
         <> metavar "STRING"
         <> value ""
         <> help "The column name for the germline sequences. \
                 \ Has preference over germline-col" )
      <*> option
          ( long "germline-col"
         <> short 'G'
         <> metavar "INT"
         <> value 1
         <> help "The column number for the germline sequences" )
      <*> strOption
          ( long "clone"
         <> short 'c'
         <> metavar "STRING"
         <> value ""
         <> help "The column name for the clone ID. Requires germline column.\
                 \ Has preference over clone-col" )
      <*> option
          ( long "clone-col"
         <> short 'C'
         <> metavar "INT"
         <> value 1
         <> help "The column number for the clone ID.\
                 \ Requires germline column" )
      <*> strOption
          ( long "label"
         <> short 'l'
         <> metavar "STRING"
         <> value ""
         <> help "An optional label to be added at the end of the header" )
      <*> strOption
          ( long "sep"
         <> short 'e'
         <> metavar "STRING"
         <> value ","
         <> help "The csv delimiter" )
      <*> switch
          ( long "no-header"
         <> short 'h'
         <> help "Whether the csv contains a header" )
      <*> switch
          ( long "include-germline"
         <> short 'p'
         <> help "Whether to include the germline in CLIP fasta style\
                 \ formatting" )
      <*> switch
          ( long "include-clone"
         <> short 'P'
         <> help "Whether to include the clones in CLIP fasta style\
                 \ formatting (needs include-germline)" )
      <*> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> value ""
         <> help "The input csv file" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value "output.fasta"
         <> help "The output fasta file" )

-- | Removes empty lines
lineCompress :: String -> String
lineCompress []        = []
lineCompress ('\n':xs) = '\n' : (lineCompress $ dropWhile (== '\n') xs)
lineCompress (x:xs)    = x : (lineCompress xs)

csvToFasta :: Options -> IO ()
csvToFasta opts = do
    contentsCarriages <- readFile . input $ opts
    -- Get rid of carriages
    let headers             = words . inputHeaders $ opts
        headerCols          = map (\x -> read x :: Int)
                            . words
                            . inputHeaderCols
                            $ opts
        seqs                = inputSeqs opts
        seqCol              = inputSeqsCol opts
        germ                = inputGerm opts
        germCol             = inputGermCol opts
        clone               = inputClone opts
        cloneCol            = inputCloneCol opts
        label               = inputLabel opts
        sep                 = if (inputSep opts == "\\t")
                                  then "\t"
                                  else inputSep opts
        contents            = lineCompress
                            . map (\x -> if (x == '\r') then '\n' else x)
                            $ contentsCarriages
        unfilteredFastaList = parseCSV
                              (noHeader opts)
                              (includeGermline opts)
                              (includeClone opts)
                              headers
                              headerCols
                              seqs
                              seqCol
                              germ
                              germCol
                              clone
                              cloneCol
                              sep
                              contents
        unlabeledFastaList  = filter (\x -> fastaSeq x /= "")
                              unfilteredFastaList
        fastaList           = if (null label)
                                  then unlabeledFastaList
                                  else map
                                       (\x
                                      -> x { fastaInfo = label
                                                      ++ "|"
                                                      ++ fastaInfo x } )
                                         unlabeledFastaList


    -- Save results
    writeFile (output opts) . printFasta (includeClone opts) $ fastaList

main :: IO ()
main = execParser opts >>= csvToFasta
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Convert a csv file to a fasta file"
     <> header "csv-to-fasta, Gregory W. Schwartz" )
