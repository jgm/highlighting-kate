module Text.Highlighting.Kate.Definitions where
import qualified Data.Map as Map

data SyntaxDefinition =
  SyntaxDefinition { synLanguage      :: String
                   , synAuthor        :: String
                   , synVersion       :: String
                   , synLicense       :: String
                   , synExtensions    :: String
                   , synCaseSensitive :: Bool
                   , synLists         :: [(String, [String])]
                   , synContexts      :: [SyntaxContext]
                   , synItemDatas     :: [(String, String)]
                   , synKeywordAttr   :: SyntaxKeywordAttr
                   } deriving (Read, Show)

data SyntaxKeywordAttr =
  SyntaxKeywordAttr  { keywordCaseSensitive   :: Bool
                     , keywordWeakDelim       :: [Char]
                     , keywordAdditionalDelim :: [Char]
                     } deriving (Read, Show)

data SyntaxContext = 
  SyntaxContext { contName               :: String
                , contAttribute          :: String
                , contLineEndContext     :: String
                , contLineBeginContext   :: String
                , contFallthrough        :: Bool
                , contFallthroughContext :: String
                , contDynamic            :: Bool
                , contParsers            :: [SyntaxParser] 
                } deriving (Read, Show)

data SyntaxParser =
  SyntaxParser { parserType              :: String
               , parserAttribute         :: String
               , parserContext           :: String
               , parserLookAhead         :: Bool
               , parserIncludeAttrib     :: Bool
               , parserFirstNonSpace     :: Bool
               , parserColumn            :: Maybe Int
               , parserDynamic           :: Bool
               , parserString            :: String -- could be a regex
               , parserChar              :: Char
               , parserChar1             :: Char
               , parserChildren          :: [SyntaxParser]
               } deriving (Read, Show)

type ContextStack = Map.Map String [String]

data SyntaxState = SyntaxState { synStContexts             :: ContextStack
                               , synStLanguage             :: String
                               , synStCurrentLine          :: String 
                               , synStCharsParsedInLine    :: Int
                               , synStCaseSensitive        :: Bool
                               , synStKeywordCaseSensitive :: Bool
                               , synStKeywordDelims        :: [Char]
                               , synStCaptures             :: [String]
                               } deriving (Read, Show)

type LabeledSource = ([String], String)

data SourceLine = SourceLine Int [LabeledSource] deriving (Read, Show, Eq)

