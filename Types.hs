-- Types module.
-- By Gregory W. Schwartz
--
-- Collects all application specific types.

module Types where

-- | Algebraic
data FastaSequence = FastaSequence { fastaInfo :: String
                                   , fastaSeq  :: String
                                   , germline  :: Maybe String
                                   , cloneID   :: Maybe String
                                   } deriving (Eq, Ord, Show)
