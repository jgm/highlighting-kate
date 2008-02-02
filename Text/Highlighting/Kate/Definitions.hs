module Text.Highlighting.Kate.Definitions where
import qualified Data.Map as Map

-- | A stack of context names for each language.  (Language-specific context
-- stacks must be maintained because of IncludeRules.)
type ContextStack = Map.Map String [String]

-- | State for syntax parser.
data SyntaxState = SyntaxState { synStContexts             :: ContextStack -- ^ Stack of contexts (top is current)
                               , synStLanguage             :: String       -- ^ Language being parsed 
                               , synStCurrentLine          :: String       -- ^ Contents of current line
                               , synStCharsParsedInLine    :: Int          -- ^ Number of characters parsed in line so far
                               , synStCaseSensitive        :: Bool         -- ^ True if language is case-sensitive
                               , synStKeywordCaseSensitive :: Bool         -- ^ True if keywords are case-sensitive
                               , synStKeywordDelims        :: [Char]       -- ^ List of keyword delimiters
                               , synStCaptures             :: [String]     -- ^ List of regex captures from last capturing match
                               } deriving (Read, Show)

-- | A pair consisting of a list of attributes and some text.
type LabeledSource = ([String], String)

-- | A line of source, including a line number and a list of labeled source items.
data SourceLine = SourceLine Int [LabeledSource] deriving (Read, Show, Eq)

