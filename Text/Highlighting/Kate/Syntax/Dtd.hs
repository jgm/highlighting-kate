{- This module was generated from data in the Kate syntax highlighting file dtd.xml, version 1.02,
   by  Andriy Lesyuk (s-andy@in.if.ua) -}

module Text.Highlighting.Kate.Syntax.Dtd ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "DTD"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.dtd"

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
  setState $ st { synStLanguage = "DTD" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("DTD",["Normal"])], synStLanguage = "DTD", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Comment" -> return ()
    "PI" -> return ()
    "Declaration" -> return ()
    "String" -> return ()
    "InlineComment" -> (popContext >> return ())
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

styles = [("Normal","Normal"),("Comment","Comment"),("Processing Instruction","Keyword"),("Declaration","DataType"),("Name","Function"),("Delimiter","DecVal"),("Symbol","Float"),("Keyword","Keyword"),("String","String"),("Entity","DecVal"),("Local","DecVal")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal"),("Comment","Comment"),("PI","Normal"),("Declaration","Normal"),("String","String"),("InlineComment","Comment")]

parseRules "Normal" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal"))
                        <|>
                        ((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pString False "<?xml" >>= withAttribute "Processing Instruction") >>~ pushContext "PI")
                        <|>
                        ((pString False "<!ELEMENT" >>= withAttribute "Declaration") >>~ pushContext "Declaration")
                        <|>
                        ((pString False "<!ATTLIST" >>= withAttribute "Declaration") >>~ pushContext "Declaration")
                        <|>
                        ((pString False "<!NOTATION" >>= withAttribute "Declaration") >>~ pushContext "Declaration")
                        <|>
                        ((pString False "<!ENTITY" >>= withAttribute "Declaration") >>~ pushContext "Declaration")
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal")))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pString False "-->" >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules "PI" = 
  do (attr, result) <- ((pDetect2Chars False '?' '>' >>= withAttribute "Processing Instruction") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Declaration" = 
  do (attr, result) <- (((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "InlineComment")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Declaration") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pRegExpr (compileRegex "(-|O)\\s(-|O)") >>= withAttribute "Declaration"))
                        <|>
                        ((pAnyChar "(|)," >>= withAttribute "Delimiter"))
                        <|>
                        ((pRegExpr (compileRegex "(%|&)(#[0-9]+|#[xX][0-9A-Fa-f]+|[\\-\\w\\d\\.:_]+);") >>= withAttribute "Entity"))
                        <|>
                        ((pAnyChar "?*+-&" >>= withAttribute "Symbol"))
                        <|>
                        ((pRegExpr (compileRegex "%\\s") >>= withAttribute "Local"))
                        <|>
                        ((pKeyword ["EMPTY","ANY","CDATA","ID","IDREF","IDREFS","NMTOKEN","NMTOKENS","ENTITY","ENTITIES","NOTATION","PUBLIC","SYSTEM","NDATA"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["#PCDATA","#REQUIRED","#IMPLIED","#FIXED"] >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[\\-\\w\\d\\.:_]+\\b") >>= withAttribute "Name")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "%[\\-\\w\\d\\.:_]+;") >>= withAttribute "Entity")))
     return (attr, result)

parseRules "InlineComment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x