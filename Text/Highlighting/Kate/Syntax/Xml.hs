{- This module was generated from data in the Kate syntax highlighting file xml.xml, version 1.97,
   by  Wilbert Berendsen (wilbert@kde.nl) -}

module Text.Highlighting.Kate.Syntax.Xml ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "XML"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.docbook;*.xml;*.rc;*.daml;*.rdf;*.rss;*.xspf"

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
  setState $ st { synStLanguage = "XML" }
  context <- currentContext <|> (pushContext "Start" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("XML",["Start"])], synStLanguage = "XML", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Start" -> return ()
    "FindXML" -> return ()
    "FindEntityRefs" -> return ()
    "FindPEntityRefs" -> return ()
    "Comment" -> return ()
    "CDATA" -> return ()
    "PI" -> return ()
    "Doctype" -> return ()
    "Doctype Internal Subset" -> return ()
    "Doctype Markupdecl" -> return ()
    "Doctype Markupdecl DQ" -> return ()
    "Doctype Markupdecl SQ" -> return ()
    "Element" -> return ()
    "El Content" -> return ()
    "El End" -> return ()
    "Attribute" -> return ()
    "Value" -> return ()
    "Value DQ" -> return ()
    "Value SQ" -> return ()
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

styles = [("Normal Text","Normal"),("Comment","Comment"),("CDATA","BaseN"),("Processing Instruction","Keyword"),("Doctype","DataType"),("Element","Keyword"),("Attribute","Others"),("Value","String"),("EntityRef","DecVal"),("PEntityRef","DecVal"),("Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Start","Normal Text"),("FindXML","Normal Text"),("FindEntityRefs","Normal Text"),("FindPEntityRefs","Normal Text"),("Comment","Comment"),("CDATA","Normal Text"),("PI","Normal Text"),("Doctype","Normal Text"),("Doctype Internal Subset","Normal Text"),("Doctype Markupdecl","Normal Text"),("Doctype Markupdecl DQ","Value"),("Doctype Markupdecl SQ","Value"),("Element","Normal Text"),("El Content","Normal Text"),("El End","Normal Text"),("Attribute","Normal Text"),("Value","Normal Text"),("Value DQ","Value"),("Value SQ","Value")]

parseRules "Start" = 
  do (attr, result) <- ((parseRules "FindXML"))
     return (attr, result)

parseRules "FindXML" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pString False "<![CDATA[" >>= withAttribute "CDATA") >>~ pushContext "CDATA")
                        <|>
                        ((pRegExpr (compileRegex "<!DOCTYPE\\s+") >>= withAttribute "Doctype") >>~ pushContext "Doctype")
                        <|>
                        ((pRegExpr (compileRegex "<\\?[\\w:_-]*") >>= withAttribute "Processing Instruction") >>~ pushContext "PI")
                        <|>
                        ((pRegExpr (compileRegex "<[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Element") >>~ pushContext "Element")
                        <|>
                        ((parseRules "FindEntityRefs"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text")))
     return (attr, result)

parseRules "FindEntityRefs" = 
  do (attr, result) <- (((pRegExpr (compileRegex "&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z_:][\\w.:_-]*);") >>= withAttribute "EntityRef"))
                        <|>
                        ((pAnyChar "&<" >>= withAttribute "Error")))
     return (attr, result)

parseRules "FindPEntityRefs" = 
  do (attr, result) <- (((pRegExpr (compileRegex "&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z_:][\\w.:_-]*);") >>= withAttribute "EntityRef"))
                        <|>
                        ((pRegExpr (compileRegex "%[A-Za-z_:][\\w.:_-]*;") >>= withAttribute "PEntityRef"))
                        <|>
                        ((pAnyChar "&%" >>= withAttribute "Error")))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pString False "-->" >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "-(-(?!->))+") >>= withAttribute "Error"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules "CDATA" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pString False "]]>" >>= withAttribute "CDATA") >>~ (popContext >> return ()))
                        <|>
                        ((pString False "]]&gt;" >>= withAttribute "EntityRef")))
     return (attr, result)

parseRules "PI" = 
  do (attr, result) <- ((pDetect2Chars False '?' '>' >>= withAttribute "Processing Instruction") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Doctype" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Doctype") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Doctype") >>~ pushContext "Doctype Internal Subset"))
     return (attr, result)

parseRules "Doctype Internal Subset" = 
  do (attr, result) <- (((pDetectChar False ']' >>= withAttribute "Doctype") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "<!(ELEMENT|ENTITY|ATTLIST|NOTATION)\\b") >>= withAttribute "Doctype") >>~ pushContext "Doctype Markupdecl")
                        <|>
                        ((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pRegExpr (compileRegex "<\\?[\\w:_-]*") >>= withAttribute "Processing Instruction") >>~ pushContext "PI")
                        <|>
                        ((parseRules "FindPEntityRefs")))
     return (attr, result)

parseRules "Doctype Markupdecl" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Doctype") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Value") >>~ pushContext "Doctype Markupdecl DQ")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Value") >>~ pushContext "Doctype Markupdecl SQ"))
     return (attr, result)

parseRules "Doctype Markupdecl DQ" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Value") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindPEntityRefs")))
     return (attr, result)

parseRules "Doctype Markupdecl SQ" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "Value") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindPEntityRefs")))
     return (attr, result)

parseRules "Element" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ pushContext "El Content")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Attribute") >>~ pushContext "Attribute")
                        <|>
                        ((pRegExpr (compileRegex "\\s+[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Attribute") >>~ pushContext "Attribute")
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "El Content" = 
  do (attr, result) <- (((pRegExpr (compileRegex "</[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Element") >>~ pushContext "El End")
                        <|>
                        ((parseRules "FindXML")))
     return (attr, result)

parseRules "El End" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "Attribute" = 
  do (attr, result) <- (((pDetectChar False '=' >>= withAttribute "Attribute") >>~ pushContext "Value")
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "Value" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Value") >>~ pushContext "Value DQ")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Value") >>~ pushContext "Value SQ")
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "Value DQ" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Value") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindEntityRefs")))
     return (attr, result)

parseRules "Value SQ" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "Value") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindEntityRefs")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
