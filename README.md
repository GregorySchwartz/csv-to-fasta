csv-to-fasta
============

**Gregory W. Schwartz**

This program will take a csv file and return a fasta file or CLIP fasta file
with germlines and / or clones.

To install:
```
cabal update
cabal install
```

```
csv-to-fasta, Gregory W. Schwartz

Usage: csv-to-fasta [-n|--headers STRING] [-N|--header-cols INT]
                    [-s|--seqs STRING] [-S|--seqs-col INT]
                    [-g|--germline STRING] [-G|--germline-col INT]
                    [-c|--clone STRING] [-C|--clone-col INT] [-l|--label STRING]
                    [-e|--sep STRING] [-h|--no-header] [-p|--include-germline]
                    [-P|--include-clone] [-i|--input FILE] [-o|--output FILE]
  Convert a csv file to a fasta file

Available options:
  -h,--help                Show this help text
  -n,--headers STRING      The column names headers separated by a space.
                           Appears in the header in the order given. Has
                           preference over header-cols
  -N,--header-cols INT     The column numbers for the header separated by a
                           space. Appears in the header in the order given
  -s,--seqs STRING         The column name for the sequences. Has preference
                           over seqs-col
  -S,--seqs-col INT        The column number for the sequences
  -g,--germline STRING     The column name for the germline sequences. Has
                           preference over germline-col
  -G,--germline-col INT    The column number for the germline sequences
  -c,--clone STRING        The column name for the clone ID. Requires germline
                           column. Has preference over clone-col
  -C,--clone-col INT       The column number for the clone ID. Requires germline
                           column
  -l,--label STRING        An optional label to be added at the end of the
                           header
  -e,--sep STRING          The csv delimiter
  -h,--no-header           Whether the csv contains a header
  -p,--include-germline    Whether to include the germline in CLIP fasta style
                           formatting
  -P,--include-clone       Whether to include the clones in CLIP fasta style
                           formatting (needs include-germline)
  -i,--input FILE          The input csv file
  -o,--output FILE         The output fasta file
```
