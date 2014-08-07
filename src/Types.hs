-- Types module.
-- By Gregory W. Schwartz
--
-- Collects all application specific types.

module Types where

-- | Cabal
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Format (shortest)

-- | Algebraic
data FastaSequence = FastaSequence { fastaInfo :: T.Text
                                   , fastaSeq  :: T.Text
                                   , germline  :: Maybe T.Text
                                   , cloneID   :: Maybe T.Text
                                   } deriving (Eq, Ord, Show)

-- So I can go straight from a number to Text or Text to number
class ShowText a where
    showText :: a -> T.Text
class ReadText a where
    readText :: T.Text -> a

instance ShowText Int where
    showText = toLazyText . decimal
instance ShowText Double where
    showText = toLazyText . shortest
