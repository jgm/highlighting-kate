{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP, DeriveDataTypeable #-}
{- |
   Module      : Text.Highlighting.Kate.Types
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Definitions for data structures needed by highlighting-kate.
-}

module Text.Highlighting.Kate.Types where
import Text.ParserCombinators.Parsec
import Data.Word
import Text.Printf
import Data.Data (Data)
import Data.Bits
import Data.Typeable (Typeable)

-- | A context: pair of syntax name and context name.
type Context = (String, String)

-- | A stack of contexts.  (Language-specific context
-- stacks must be maintained because of IncludeRules.)
type ContextStack = [Context]

-- | State for syntax parser.
data SyntaxState = SyntaxState
  { synStContexts             :: ContextStack -- ^ Stack of contexts
  , synStLineNumber           :: Int          -- ^ Number of current line
  , synStPrevChar             :: Char         -- ^ Last character parsed
  , synStPrevNonspace         :: Bool         -- ^ True if we've parsed a nonspace
  , synStContinuation         :: Bool         -- ^ True if last thing parsed is
                                              --   a LineContinue
  , synStCaseSensitive        :: Bool         -- ^ Language is case-sensitive
  , synStKeywordCaseSensitive :: Bool         -- ^ Keywords are case-sensitive
  , synStCaptures             :: [String]     -- ^ List of regex captures from
                                              --   last capturing match
  } deriving Show

defaultSyntaxState :: SyntaxState
defaultSyntaxState = SyntaxState{
    synStContexts = []
  , synStLineNumber = 0
  , synStPrevNonspace = False
  , synStContinuation = False
  , synStPrevChar = '\n'
  , synStCaseSensitive = True
  , synStKeywordCaseSensitive = True
  , synStCaptures = []
  }

-- | A pair consisting of a list of attributes and some text.
type Token = (TokenType, String)

data TokenType = KeywordTok
               | DataTypeTok
               | DecValTok
               | BaseNTok
               | FloatTok
               | ConstantTok
               | CharTok
               | SpecialCharTok
               | StringTok
               | VerbatimStringTok
               | SpecialStringTok
               | ImportTok
               | CommentTok
               | DocumentationTok
               | AnnotationTok
               | CommentVarTok
               | OtherTok
               | FunctionTok
               | VariableTok
               | ControlFlowTok
               | OperatorTok
               | BuiltInTok
               | ExtensionTok
               | PreprocessorTok
               | AttributeTok
               | RegionMarkerTok
               | InformationTok
               | WarningTok
               | AlertTok
               | ErrorTok
               | NormalTok
               deriving (Read, Show, Eq, Enum, Data, Typeable)

-- | A line of source, list of labeled source items.
type SourceLine = [Token]

type KateParser = GenParser Char SyntaxState

data TokenStyle = TokenStyle {
    tokenColor      :: Maybe Color
  , tokenBackground :: Maybe Color
  , tokenBold       :: Bool
  , tokenItalic     :: Bool
  , tokenUnderline  :: Bool
  } deriving (Show, Read, Data, Typeable)

defStyle :: TokenStyle
defStyle = TokenStyle {
    tokenColor      = Nothing
  , tokenBackground = Nothing
  , tokenBold       = False
  , tokenItalic     = False
  , tokenUnderline  = False
  }

data Color = RGB Word8 Word8 Word8 deriving (Show, Read, Data, Typeable)

class ToColor a where
  toColor :: a -> Maybe Color

instance ToColor String where
  toColor ['#',r1,r2,g1,g2,b1,b2] =
     case reads ['(','0','x',r1,r2,',','0','x',g1,g2,',','0','x',b1,b2,')'] of
           ((r,g,b),_) : _ -> Just $ RGB r g b
           _                                         -> Nothing
  toColor _        = Nothing

instance ToColor Int where
  toColor x = toColor (fromIntegral x1 :: Word8,
                       fromIntegral x2 :: Word8,
                       fromIntegral x3 :: Word8)
    where x1 = (shiftR x 16) .&. 0xFF
          x2 = (shiftR x 8 ) .&. 0xFF
          x3 = x             .&. 0xFF

instance ToColor (Word8, Word8, Word8) where
  toColor (r,g,b) = Just $ RGB r g b

instance ToColor (Double, Double, Double) where
  toColor (r,g,b) | r >= 0 && g >= 0 && b >= 0 && r <= 1 && g <= 1 && b <= 1 =
          Just $ RGB (floor $ r * 255) (floor $ g * 255) (floor $ b * 255)
  toColor _ = Nothing

class FromColor a where
  fromColor :: Color -> a

instance FromColor String where
  fromColor (RGB r g b) = printf "#%02x%02x%02x" r g b

instance FromColor (Double, Double, Double) where
  fromColor (RGB r g b) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

instance FromColor (Word8, Word8, Word8) where
  fromColor (RGB r g b) = (r, g, b)

data Style = Style {
    tokenStyles               :: [(TokenType, TokenStyle)]
  , defaultColor              :: Maybe Color
  , backgroundColor           :: Maybe Color
  , lineNumberColor           :: Maybe Color
  , lineNumberBackgroundColor :: Maybe Color
  } deriving (Read, Show, Data, Typeable)

-- | Options for formatting source code.
data FormatOptions = FormatOptions{
         numberLines      :: Bool     -- ^ Number lines
       , startNumber      :: Int      -- ^ Number of first line
       , lineAnchors      :: Bool     -- ^ Anchors on each line number
       , titleAttributes  :: Bool     -- ^ Html titles with token types
       , codeClasses      :: [String] -- ^ Additional classes for Html code tag
       , containerClasses :: [String] -- ^ Additional classes for Html container tag
                                      --   (pre or table depending on numberLines)
       } deriving (Eq, Show, Read)

defaultFormatOpts :: FormatOptions
defaultFormatOpts = FormatOptions{
                      numberLines = False
                    , startNumber = 1
                    , lineAnchors = False
                    , titleAttributes = False
                    , codeClasses = []
                    , containerClasses = []
                    }
