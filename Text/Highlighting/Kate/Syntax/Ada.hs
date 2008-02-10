{- This module was generated from data in the Kate syntax highlighting file ada.xml, version 1.07,
   by   -}

module Text.Highlighting.Kate.Syntax.Ada ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Ada"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.adb;*.ads;*.ada;*.a"

-- | Highlight source code using this syntax definition.
highlight :: String -> Either String [SourceLine]
highlight input =
  case runParser parseSource startingState "source" input of
    Left err     -> Left $ show err
    Right result -> Right result

-- | Parse an expression using appropriate local context.
parseExpression :: GenParser Char SyntaxState LabeledSource
parseExpression = do
  st <- getState
  let oldLang = synStLanguage st
  setState $ st { synStLanguage = "Ada" }
  context <- currentContext <|> (pushContext "Default" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Ada",["Default"])], synStLanguage = "Ada", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Default" -> return ()
    "Region Marker" -> (popContext >> return ())
    "String" -> (popContext >> return ())
    "Comment" -> (popContext >> return ())
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }

withAttribute attr txt = do
  if null txt
     then fail "Parser matched no text"
     else return ()
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Pragmas","Keyword"),("Data Type","DataType"),("Decimal","DecVal"),("Base-N","BaseN"),("Float","Float"),("Char","Char"),("String","String"),("Comment","Comment"),("Symbol","Normal"),("Region Marker","RegionMarker")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Default","Normal Text"),("Region Marker","Region Marker"),("String","String"),("Comment","Comment")]

parseRules "Default" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\brecord\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s+record\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bcase\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s+case\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bif\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s+if\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bloop\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s+loop\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bselect\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s+select\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bbegin\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pFirstNonSpace >> pString False "--  BEGIN" >>= withAttribute "Region Marker") >>~ pushContext "Region Marker")
                        <|>
                        ((pFirstNonSpace >> pString False "--  END" >>= withAttribute "Region Marker") >>~ pushContext "Region Marker")
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["abort","abs","abstract","accept","access","aliased","all","and","array","at","begin","body","constant","declare","delay","delta","digits","do","else","elsif","end","entry","exception","exit","for","function","generic","goto","in","interface","is","limited","mod","new","not","null","of","or","others","out","overriding","package","pragma","private","procedure","protected","raise","range","rem","record","renames","requeue","return","reverse","separate","subtype","tagged","task","terminate","then","type","until","use","when","while","with","xor"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["all_calls_remote","assert","assertion_policy","asynchronous","atomic","atomic_components","attach_handler","controlled","convention","detect_blocking","discard_names","elaborate","elaborate_all","elaborate_body","export","import","inline","inspection_point","interrupt_handler","interrupt_priority","linker_options","list","locking_policy","no_return","normalize_scalars","optimize","pack","page","partition_elaboration_policy","preelaborable_initialization","preelaborate","priority","priority_specific_dispatching","profile","pure","queuing_policy","relative_deadline","remote_call_interface","remote_types","restrictions","reviewable","shared_passive","storage_size","suppress","task_dispatching_policy","unchecked_union","unsuppress","volatile","volatile_components"] >>= withAttribute "Pragmas"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["boolean","char","float","integer","long_float","long_integer","long_long_float","long_long_integer","short_float","short_integer","string","wide_string","wide_char","wide_wide_char","wide_wide_string"] >>= withAttribute "Data Type"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pRegExpr (compileRegex "'.'") >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pAnyChar ":!%&()+,-/.*<=>|" >>= withAttribute "Symbol")))
     return (attr, result)

parseRules "Region Marker" = 
  pzero

parseRules "String" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Comment" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x
