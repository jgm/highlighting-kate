{- This module was generated from data in the Kate syntax highlighting file relaxngcompact.xml, version 0.2,
   by  Rintze Zelle -}

module Text.Highlighting.Kate.Syntax.Relaxngcompact ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "RelaxNG-Compact"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.rnc"

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
  setState $ st { synStLanguage = "RelaxNG-Compact" }
  context <- currentContext <|> (pushContext "Normal Text" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("RelaxNG-Compact",["Normal Text"])], synStLanguage = "RelaxNG-Compact", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal Text" -> return ()
    "Comments" -> (popContext >> return ())
    "String" -> return ()
    "Node Names" -> (popContext >> return ())
    "Definitions" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Comments","Comment"),("String","String"),("Keywords","Keyword"),("Datatypes","DataType"),("Node Names","Others"),("Definitions","Function")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("Comments","Comments"),("String","String"),("Node Names","Node Names"),("Definitions","Definitions")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pFirstNonSpace >> pDetectChar False '#' >>= withAttribute "Comments") >>~ pushContext "Comments")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pKeyword " \n\t.()!+,<=>%&*/;?[]^{|}~\\" ["default","datatypes","div","empty","external","grammar","include","inherit","list","mixed","namespace","notAllowed","parent","start","token"] >>= withAttribute "Keywords"))
                        <|>
                        ((pKeyword " \n\t.()!+,<=>%&*/;?[]^{|}~\\" ["attribute","element"] >>= withAttribute "Keywords") >>~ pushContext "Node Names")
                        <|>
                        ((pKeyword " \n\t.()!+,<=>%&*/;?[]^{|}~\\" ["string","text","xsd:anyURI","xsd:base64Binary","xsd:boolean","xsd:byte","xsd:date","xsd:dateTime","xsd:decimal","xsd:double","xsd:duration","xsd:ENTITIES","xsd:ENTITY","xsd:float","xsd:gDay","xsd:gMonth","xsd:gMonthDay","xsd:gYear","xsd:gYearMonth","xsd:hexBinary","xsd:ID","xsd:IDREF","xsd:IDREFS","xsd:int","xsd:integer","xsd:language","xsd:long","xsd:Name","xsd:NCName","xsd:negativeInteger","xsd:NMTOKEN","xsd:NMTOKENS","xsd:nonNegativeInteger","xsd:nonPositiveInteger","xsd:normalizedString","xsd:NOTATION","xsd:positiveInteger","xsd:QName","xsd:short","xsd:string","xsd:time","xsd:token","xsd:unsignedByte","xsd:unsignedInt","xsd:unsignedLong","xsd:unsignedShort"] >>= withAttribute "Datatypes"))
                        <|>
                        ((lookAhead (pRegExpr (compileRegex "[\\w\\.-]+[\\s]+=")) >> return ([],"") ) >>~ pushContext "Definitions"))
     return (attr, result)

parseRules "Comments" = 
  pzero

parseRules "String" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Node Names" = 
  do (attr, result) <- ((lookAhead (pDetectChar False '{') >> return ([],"") ) >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Definitions" = 
  do (attr, result) <- ((lookAhead (pDetectChar False '=') >> return ([],"") ) >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
