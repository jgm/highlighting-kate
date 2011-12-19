{- |
   Module      : Text.Highlighting.Kate.Definitions
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Definitions for data structures needed by highlighting-kate.
-}

module Text.Highlighting.Kate.Definitions where
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

-- | A stack of context names for each language.  (Language-specific context
-- stacks must be maintained because of IncludeRules.)
type ContextStack = Map.Map String [String]

-- | State for syntax parser.
data SyntaxState = SyntaxState
  { synStContexts             :: ContextStack -- ^ Stack of contexts
  , synStLanguage             :: String       -- ^ Language being parsed 
  , synStCurrentLine          :: String       -- ^ Contents of current line
  , synStCharsParsedInLine    :: Int          -- ^ Num characters parsed in line
  , synStPrevChar             :: Char         -- ^ Last character parsed
  , synStCaseSensitive        :: Bool         -- ^ Language is case-sensitive
  , synStKeywordCaseSensitive :: Bool         -- ^ Keywords are case-sensitive
  , synStCaptures             :: [String]     -- ^ List of regex captures from
                                              --   last capturing match
  } deriving (Read, Show)

-- | A pair consisting of a list of attributes and some text.
type Token = (TokenType, String)

data TokenType = KeywordTok
               | DataTypeTok
               | DecValTok
               | BaseNTok
               | FloatTok
               | CharTok
               | StringTok
               | CommentTok
               | OtherTok
               | AlertTok
               | FunctionTok
               | RegionMarkerTok
               | ErrorTok
               | NormalTok
               deriving (Read, Show, Eq)

-- | A line of source, list of labeled source items.
type SourceLine = [Token]

type KateParser = GenParser Char SyntaxState
