{-# OPTIONS -farrows #-}

{- 
ParseSyntaxFiles.hs processes a directory containing Kate
XML syntax highlighting definitions.  For each xml file in
the directory, it creates a syntax highlighting parser

Text/Highlighting/Kate/Syntax/(name).hs.

Finally, it creates a module

Text/Highlighting/Kate/Syntax.hs with a wrapper around

all these modules.

Usage:  runghc ParseSyntaxFiles.hs xml

Requires HXT.
-}

module Main where

import Text.XML.HXT.Arrow
import Text.XML.HXT.Arrow.Edit
import Text.XML.HXT.Arrow.XmlNodeSet
import Control.Arrow
import Control.Arrow.ArrowList
import Data.List
import Data.Maybe
import Data.Char (toUpper, toLower, isAlphaNum)
import qualified Data.Map as Map
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.PrettyPrint
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common (capitalize)

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
                     , keywordDelims          :: [Char]
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


libraryPath = joinPath ["Text", "Highlighting", "Kate"]
destDir = joinPath [libraryPath, "Syntax"]

main :: IO ()
main = do
  argv <- getArgs
  srcDir <- if null argv 
               then error "Specify path of directory containing xml syntax files."
               else return $ argv !! 0
  let isXmlFile x = isSuffixOf ".xml" x
  files <- getDirectoryContents srcDir >>= (return . map (combine srcDir) . filter isXmlFile)
  destDirExists <- doesDirectoryExist destDir
  if destDirExists
     then return ()
     else createDirectory destDir 
  mapM processOneFile files >> return ()
  let syntaxFile = combine libraryPath (addExtension "Syntax" "hs")
  putStrLn $ "Writing " ++ syntaxFile
  let names = sort $ map nameFromPath files 
  let imports = unlines $ map (\name -> "import qualified Text.Highlighting.Kate.Syntax." ++ name ++ " as " ++ name) names 
  let cases = unlines $ map (\name -> "        " ++ show (map toLower name) ++ " -> " ++ name ++ ".highlight") names
  let languageExtensions = concat $ intersperse ", " $ map (\name -> "(" ++ show name ++ ", " ++ name ++ ".syntaxExtensions)") names
  writeFile syntaxFile $
           "module Text.Highlighting.Kate.Syntax ( highlightAs, languages, languagesByExtension ) where\n\
           \import Data.Char (toLower)\n\
           \import Data.Maybe (fromMaybe)\n\
           \import Text.Highlighting.Kate.Definitions\n" ++
           imports ++ "\n" ++
           "-- | List of supported languages.\n\
           \languages :: [String]\n\
           \languages = " ++ show names ++ "\n\n\
           \-- | List of language extensions.\n\
           \languageExtensions :: [(String, String)]\n\
           \languageExtensions = [" ++ languageExtensions ++ "]\n\n" ++
           "-- | Returns a list of languages appropriate for the given file extension.\n\
           \languagesByExtension :: String -> [String]\n\
           \languagesByExtension ext = filter (hasExtension ext) languages\n\n\
           \-- | True if extension belongs to language.\n\
           \hasExtension ext lang =\n\
           \  let exts = fromMaybe \"\" (lookup lang languageExtensions)\n\
           \      matchExtension _ [] = False\n\
           \      matchExtension ext ('.':xs) =\n\
           \        let (next, rest) = span (/=';') xs\n\
           \        in  if next == ext then True else matchExtension ext rest\n\
           \      matchExtension ext (_:xs) = matchExtension ext xs\n\
           \  in  matchExtension (dropWhile (=='.') ext) exts\n\n\
           \-- | Highlight source code using a specified syntax definition.\n\
           \highlightAs :: String                        -- ^ Language syntax\n\
           \            -> String                        -- ^ Source code to highlight\n\
           \            -> Either String [SourceLine]    -- ^ Either error message or result\n\
           \highlightAs lang =\n\
           \  case (map toLower lang) of\n" ++
           cases ++
           "        _ -> (\\_ -> Left (\"Unknown language ++ \" ++ lang))\n"

processOneFile :: FilePath -> IO ()
processOneFile src = do
  [syntax] <- runX $ application src
  let name = nameFromPath src
  let outFile = joinPath [libraryPath, "Syntax", addExtension name "hs"]
  let includeLangs = nub $ map (drop 2 . parserContext) $
        filter (\p -> (parserType p) == "IncludeRules" && "##" `isPrefixOf` (parserContext p)) $ 
        concatMap contParsers $ synContexts syntax
  let includeImports = map (("import qualified " ++) . langNameToModule) includeLangs
  putStrLn $ "Writing " ++ outFile
  writeFile outFile $ 
           "{- This module was generated from data in the Kate syntax highlighting file " ++ (takeFileName src) ++ ", version " ++ 
           synVersion syntax ++ ",\n" ++
           "   by  " ++ synAuthor syntax ++ " -}\n\n" ++ 
           "module Text.Highlighting.Kate.Syntax." ++ name ++ " ( highlight, parseExpression, syntaxName, syntaxExtensions ) where\n\
           \import Text.Highlighting.Kate.Definitions\n\
           \import Text.Highlighting.Kate.Common\n" ++
           unlines includeImports ++ 
           "import Text.ParserCombinators.Parsec\n\
           \import Data.List (nub)\n\
           \import Data.Map (fromList)\n\
           \import Data.Maybe (fromMaybe)\n\n" ++
           render (mkParser syntax) ++ "\n"

mkParser :: SyntaxDefinition -> Doc
mkParser syntax = 
  let name = text "-- | Full name of language." $$
             text "syntaxName :: String" $$
             text ("syntaxName = " ++ show (synLanguage syntax))
      exts = text "-- | Filename extensions for this language." $$
             text "syntaxExtensions :: String" $$
             text ("syntaxExtensions = " ++ show (synExtensions syntax))
      styles = text ("styles = " ++ (show $ map (\(typ, sty) -> (typ, drop 2 sty)) $ synItemDatas syntax))
      withAttr = text "withAttribute attr txt = do" $$ (nest 2 $
                   text "if null txt" $$
                   text "   then fail \"Parser matched no text\"" $$
                   text "   else return ()" $$
                   text "let style = fromMaybe \"\" $ lookup attr styles" $$
                   text "st <- getState" $$
                   text "let oldCharsParsed = synStCharsParsedInLine st" $$
                   text "updateState $ \\st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } " $$
                   text "return (nub [style, attr], txt)")
      parseExpressionInternal = text "parseExpressionInternal = do" $$ (nest 2 $ 
                                  text "context <- currentContext" $$
                                  text "parseRules context <|> (pDefault >>= withAttribute (fromMaybe \"\" $ lookup context defaultAttributes))")
      parseExpression = text "-- | Parse an expression using appropriate local context." $$
                        text "parseExpression :: GenParser Char SyntaxState LabeledSource" $$
                        text "parseExpression = do" $$ (nest 2 $ 
                          text "st <- getState" $$
                          text "let oldLang = synStLanguage st" $$
                          text ("setState $ st { synStLanguage = " ++ show (synLanguage syntax) ++ " }") $$
                          text ("context <- currentContext <|> (pushContext " ++ show (contName $ head $ synContexts syntax) ++
                                " >> currentContext)") $$
                          text "result <- parseRules context" $$
                          text "updateState $ \\st -> st { synStLanguage = oldLang }" $$
                          text "return result")
      defaultAttributes = text $ "defaultAttributes = " ++ (show $ map (\cont -> (contName cont, contAttribute cont)) $ synContexts syntax)
      -- Note: lineBeginContexts seems not to be used in any of the xml files
      -- lineBeginContexts = 
      --   text $ "lineBeginContexts = " ++ (show $ map (\cont -> (contName cont, contLineBeginContext cont)) $ synContexts syntax)
      startingContext = head (synContexts syntax)
      contextCatchAll = text $ "parseRules x = fail $ \"Unknown context\" ++ x"
      contexts = map (mkRules syntax) $ synContexts syntax
      initialContextStack = Map.fromList [(synLanguage syntax, [contName startingContext])]
      startingState = SyntaxState { synStContexts = initialContextStack
                                  , synStLanguage = synLanguage syntax
                                  , synStCurrentLine = ""
                                  , synStCharsParsedInLine = 0
                                  , synStCaseSensitive = synCaseSensitive syntax
                                  , synStKeywordCaseSensitive = keywordCaseSensitive $ synKeywordAttr syntax
                                  , synStCaptures = [] }
      initState = text $ "startingState = " ++ show startingState
      sourceLineParser = text "parseSourceLine = manyTill parseExpressionInternal pEndLine"
      mainParser = text "parseSource = do " $$  
                   (nest 2 $ text "lineContents <- lookAhead wholeLine" $$
                             text "updateState $ \\st -> st { synStCurrentLine = lineContents }" $$
                             -- text "context <- currentContext" $$
                             -- text "pushContext (fromMaybe \"#stay\" $ lookup context lineBeginContexts)" $$
                             text "result <- manyTill parseSourceLine eof" $$
                             text "return $ map normalizeHighlighting result")
      mainFunction = text $ "-- | Highlight source code using this syntax definition.\n\
                            \highlight :: String -> Either String [SourceLine]\n\
                            \highlight input =\n\
                            \  case runParser parseSource startingState \"source\" input of\n\
                            \    Left err     -> Left $ show err\n\
                            \    Right result -> Right result"
      endLineParser = text "pEndLine = do" $$
                      (nest 2 $ text "newline <|> (eof >> return '\\n')" $$
                                text "context <- currentContext" $$
                                text "case context of" $$
                                (nest 2 $ (vcat $ map (\cont -> text (show $ contName cont) <> text " -> " <> 
                                            switchContext (contLineEndContext cont)) $ synContexts syntax) $$
                                          (text $ "_ -> return ()")) $$
                                {- text "pushContext (fromMaybe \"#stay\" $ lookup context lineBeginContexts)" $$ -}
                                text "lineContents <- lookAhead wholeLine" $$
                                text "updateState $ \\st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }")
  in  vcat $ intersperse (text "") $ [name, exts, mainFunction, parseExpression, mainParser, initState, sourceLineParser, 
                                      endLineParser, withAttr, styles, parseExpressionInternal, 
                                      defaultAttributes {- , lineBeginContexts -}] ++ contexts ++ [contextCatchAll]

mkIdentifier :: String -> String
mkIdentifier "" = ""
mkIdentifier ('-':x:xs) = toUpper x : mkIdentifier xs
mkIdentifier ('-':xs) = mkIdentifier xs
mkIdentifier (x:xs) = x : mkIdentifier xs

mkAlternatives :: [Doc] -> Doc
mkAlternatives docs = 
  let contents = vcat $ intersperse (text "<|>") docs
  in  if length docs > 1
         then char '(' <> contents <> char ')'
         else contents

mkRules :: SyntaxDefinition -> SyntaxContext -> Doc
mkRules syntax context =
  let fallthroughParser = if contFallthrough context
                             then [parens (switchContext (contFallthroughContext context) <> 
                                   text " >> return ([], \"\")")]
                             else []
  in  text ("parseRules " ++ show (contName context) ++ " = ") $$ 
      if null (contParsers context) && null fallthroughParser
         then nest 2 (text "pzero")
         else nest 2 $ (text "do (attr, result) <- " <> 
                       (mkAlternatives $ (map (mkSyntaxParser syntax context) $ contParsers context) ++ fallthroughParser)) $$
                       text ("   return (attr, result)")

mkSyntaxParser :: SyntaxDefinition -> SyntaxContext -> SyntaxParser -> Doc
mkSyntaxParser syntax context parser = 
  let mainParser = text $ case parserType parser of
            "DetectChar"       -> "pDetectChar " ++ show (parserDynamic parser) ++ " " ++ show (parserChar parser)
            "Detect2Chars"     -> "pDetect2Chars " ++ show (parserDynamic parser) ++ " " ++ 
                                    show (parserChar parser) ++ " " ++ show (parserChar1 parser)
            "AnyChar"          -> "pAnyChar " ++ show (parserString parser)
            "StringDetect"     -> "pString " ++ show (parserDynamic parser) ++ " " ++ show (parserString parser) 
            "RegExpr"          -> if parserDynamic parser
                                     then "pRegExprDynamic " ++ show (parserString parser)
                                     else "pRegExpr (compileRegex " ++ show (parserString parser) ++ ")"
            "keyword"          -> "pKeyword " ++ show (keywordDelims $ synKeywordAttr syntax) ++ " " ++ 
                                                 show (fromMaybe [] $ lookup (parserString parser) (synLists syntax))
            "Int"              -> "pInt"
            "Float"            -> "pFloat"
            "HlCOct"           -> "pHlCOct"
            "HlCHex"           -> "pHlCHex"
            "HlCStringChar"    -> "pHlCStringChar"
            "HlCChar"          -> "pHlCChar"
            "RangeDetect"      -> "pRangeDetect " ++ show (parserChar parser) ++ " " ++ show (parserChar1 parser)
            "LineContinue"     -> "pLineContinue"
            "IncludeRules"     -> case parserContext parser of
                                      ('#':'#':xs) -> langNameToModule xs ++ ".parseExpression" ++
                                                      if parserIncludeAttrib parser
                                                         then ""
                                                         else " >>= ((withAttribute " ++ show (parserAttribute parser) ++ ") . snd)" 
                                      xs           -> "parseRules " ++ show xs
            "DetectSpaces"     -> "pDetectSpaces"
            "DetectIdentifier" -> "pDetectIdentifier"
            _                  -> "pUnimplemented"
      parserDoc = char '(' <>
                  (case (parserColumn parser) of
                       Just c  -> text $ "pColumn " ++ show c ++ " >> "
                       _       -> empty) <>
                  (if parserFirstNonSpace parser
                     then text "pFirstNonSpace >> "
                     else empty) <>
                  if parserType parser == "IncludeRules"
                     then mainParser <> char ')'
                     else (if parserLookAhead parser
                             then text "lookAhead (" <> mainParser <> text ") >> return ([],\"\") " 
                             else mainParser <> text " >>= withAttribute " <> 
                                  text (if null (parserAttribute parser)
                                           then show (contAttribute context)
                                           else show (parserAttribute parser))) <> 
                          char ')' <>
                          (if parserContext parser `elem` ["", "#stay"]
                              then empty 
                              else text " >>~ " <> switchContext (parserContext parser))
      childParsers = parserChildren parser
  in  char '(' <>
      (if null childParsers
          then parserDoc
          else text "withChildren " <> parserDoc <> char ' ' <> (mkAlternatives $ map (mkSyntaxParser syntax context) childParsers)) <>
      char ')'

switchContext next =
  case next of
     x | "#pop" `isPrefixOf` x -> char '(' <> text (concat $ intersperse " >> " $ replicate (length (filter (=='#') x)) "popContext") <> 
                                  text " >> return ())"
     "#stay" -> text "return ()"
     x -> text ("pushContext " ++ show x) 

langNameToModule str =  "Text.Highlighting.Kate.Syntax." ++
  case str of
    "Alerts" -> "Alert"
    "C++" -> "Cpp"
    "CSS" -> "Css"
    "Doxygen" -> "Doxygen"
    "HTML" -> "Html"
    "Javadoc" -> "Javadoc"
    "JavaScript" -> "Javascript"
    x -> x

nameFromPath :: FilePath -> String
nameFromPath = concat . map capitalize . words . 
               (map (\c -> if c == '-' then ' ' else c)) . takeFileName . 
               dropExtension

application :: String -> IOSArrow b SyntaxDefinition
application src
    = readDocument [(a_validate, v_0)] src
      >>>
      multi (hasName "language")
      >>>
      extractSyntaxDefinition

extractSyntaxDefinition :: IOSArrow XmlTree SyntaxDefinition
extractSyntaxDefinition =  proc x -> do
                             lang <- getAttrValue "name" -< x
                             author <- getAttrValue "author" -< x
                             version <- getAttrValue "version" -< x
                             license <- getAttrValue "license" -< x
                             sources <- getAttrValue "extensions" -< x
                             caseSensitive <- getAttrValue "casesensitive" -< x
                             itemdatas <- getItemDatas -< x
                             lists <- getLists -< x
                             contexts <- getContexts -< x
                             keywordAttr <- getKeywordAttrs -< x
                             returnA -< SyntaxDefinition { synLanguage      = lang 
                                                         , synAuthor        = author
                                                         , synVersion       = version
                                                         , synLicense       = license 
                                                         , synExtensions    = sources
                                                         , synCaseSensitive = vBool True caseSensitive
                                                         , synLists         = lists
                                                         , synContexts      = contexts 
                                                         , synItemDatas     = itemdatas
                                                         , synKeywordAttr   = if null keywordAttr
                                                                                 then defaultKeywordAttr
                                                                                 else head keywordAttr }

getItemDatas :: IOSArrow XmlTree [(String,String)]
getItemDatas = multi (hasName "itemDatas")
               >>>
               (listA $ getChildren
                       >>>
                       hasName "itemData"
                       >>>
                       getAttrValue "name" &&& getAttrValue "defStyleNum")

getLists :: IOSArrow XmlTree [(String, [String])]
getLists = listA $ multi (hasName "list")
                   >>> 
                   getAttrValue "name" &&& getListContents

getListContents :: IOSArrow XmlTree [String]
getListContents = listA $ getChildren
                          >>> 
                          hasName "item"
                          >>>
                          getChildren
                          >>>
                          getText
                          >>>
                          arr stripWhitespace

getContexts :: IOSArrow XmlTree [SyntaxContext]
getContexts = listA $   multi (hasName "context")
                        >>>
                        proc x -> do
                          name <- getAttrValue "name" -< x
                          attribute <- getAttrValue "attribute" -< x
                          lineEndContext <- getAttrValue "lineEndContext" -< x
                          lineBeginContext <- getAttrValue "lineBeginContext" -< x
                          fallthrough <- getAttrValue "fallthrough" -< x
                          fallthroughContext <- getAttrValue "fallthroughContext" -< x
                          dynamic <- getAttrValue "dynamic" -< x
                          parsers <- getParsers -< x
                          returnA -< SyntaxContext 
                                           { contName = name
                                           , contAttribute = attribute
                                           , contLineEndContext = if null lineEndContext then "#stay" else lineEndContext
                                           , contLineBeginContext = if null lineBeginContext then "#stay" else lineBeginContext
                                           , contFallthrough = vBool False fallthrough
                                           , contFallthroughContext = if null fallthroughContext then "#stay" else fallthroughContext
                                           , contDynamic = vBool False dynamic
                                           , contParsers = parsers }

getParsers :: IOSArrow XmlTree [SyntaxParser]
getParsers = listA $ getChildren 
                     >>>
                     proc x -> do
                       name <- getName -< x
                       attribute <- getAttrValue "attribute" -< x
                       context <- getAttrValue "context" -< x
                       char0 <- getAttrValue "char" -< x
                       char1 <- getAttrValue "char1" -< x
                       str <- getAttrValue "String" -< x
                       includeAttrib <- getAttrValue "includeAttrib" -< x
                       lookahead <- getAttrValue "lookAhead" -< x
                       firstNonSpace <- getAttrValue "firstNonSpace" -< x
                       column <- getAttrValue "column" -< x
                       dynamic <- getAttrValue "dynamic" -< x
                       children <- getParsers -< x
                       let tildeRegex = name == "RegExpr" && length str > 0 && head str == '^'
                       returnA -< SyntaxParser 
                                    { parserType = name
                                    , parserAttribute = attribute
                                    , parserContext = context
                                    , parserLookAhead = vBool False lookahead
                                    , parserIncludeAttrib = vBool False includeAttrib
                                    , parserFirstNonSpace = vBool False firstNonSpace
                                    , parserColumn = if tildeRegex
                                                        then Just 0
                                                        else if null column 
                                                                then Nothing 
                                                                else Just (read column)
                                    , parserDynamic = vBool False dynamic
                                    , parserString = if tildeRegex then drop 1 str else str
                                    , parserChar = if length char0 == 1 then head char0 else '*'
                                    , parserChar1 = if length char1 == 1 then head char1 else '*'
                                    , parserChildren = children }

getKeywordAttrs :: IOSArrow XmlTree [SyntaxKeywordAttr]
getKeywordAttrs = listA $ multi $ hasName "keywords"
                                  >>>
                                  proc x -> do
                                    caseSensitive <- getAttrValue "casesensitive" -< x
                                    weakDelim <- getAttrValue "weakDeliminator" -< x
                                    additionalDelim <- getAttrValue "additionalDeliminator" -< x
                                    returnA -< SyntaxKeywordAttr 
                                                      { keywordCaseSensitive = vBool True caseSensitive
                                                      , keywordDelims = (standardDelims ++ additionalDelim) \\ weakDelim }

standardDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" 

defaultKeywordAttr = SyntaxKeywordAttr { keywordCaseSensitive = True
                                       , keywordDelims = standardDelims }

stripWhitespaceLeft = dropWhile isWhitespace 
isWhitespace x = elem x [' ', '\t', '\n']
stripWhitespace = reverse . stripWhitespaceLeft . reverse . stripWhitespaceLeft

vBool :: Bool -> String -> Bool
vBool defaultVal value = case value of
                           z | z `elem` ["true","yes","1"] -> True
                           z | z `elem` ["false","no","0"] -> False
                           _ -> defaultVal

