{- This module was generated from data in the Kate syntax highlighting file html.xml, version 1.99,
   by  Wilbert Berendsen (wilbert@kde.nl) -}

module Text.Highlighting.Kate.Syntax.Html ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import qualified Text.Highlighting.Kate.Syntax.Css
import qualified Text.Highlighting.Kate.Syntax.Javascript
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "HTML"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.htm;*.html;*.shtml;*.shtm"

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
  setState $ st { synStLanguage = "HTML" }
  context <- currentContext <|> (pushContext "Start" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("HTML",["Start"])], synStLanguage = "HTML", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Start" -> return ()
    "FindHTML" -> return ()
    "FindEntityRefs" -> return ()
    "FindPEntityRefs" -> return ()
    "FindAttributes" -> return ()
    "FindDTDRules" -> return ()
    "Comment" -> return ()
    "CDATA" -> return ()
    "PI" -> return ()
    "Doctype" -> return ()
    "Doctype Internal Subset" -> return ()
    "Doctype Markupdecl" -> return ()
    "Doctype Markupdecl DQ" -> return ()
    "Doctype Markupdecl SQ" -> return ()
    "El Open" -> return ()
    "El Close" -> return ()
    "El Close 2" -> return ()
    "El Close 3" -> return ()
    "CSS" -> return ()
    "CSS content" -> return ()
    "JS" -> return ()
    "JS content" -> return ()
    "JS comment close" -> (popContext >> return ())
    "Value" -> return ()
    "Value NQ" -> (popContext >> popContext >> return ())
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

defaultAttributes = [("Start","Normal Text"),("FindHTML","Normal Text"),("FindEntityRefs","Normal Text"),("FindPEntityRefs","Normal Text"),("FindAttributes","Normal Text"),("FindDTDRules","Normal Text"),("Comment","Comment"),("CDATA","Normal Text"),("PI","Normal Text"),("Doctype","Normal Text"),("Doctype Internal Subset","Normal Text"),("Doctype Markupdecl","Normal Text"),("Doctype Markupdecl DQ","Value"),("Doctype Markupdecl SQ","Value"),("El Open","Normal Text"),("El Close","Normal Text"),("El Close 2","Normal Text"),("El Close 3","Normal Text"),("CSS","Normal Text"),("CSS content","Normal Text"),("JS","Normal Text"),("JS content","Normal Text"),("JS comment close","Comment"),("Value","Normal Text"),("Value NQ","Normal Text"),("Value DQ","Value"),("Value SQ","Value")]

parseRules "Start" = 
  do (attr, result) <- ((parseRules "FindHTML"))
     return (attr, result)

parseRules "FindHTML" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pString False "<![CDATA[" >>= withAttribute "CDATA") >>~ pushContext "CDATA")
                        <|>
                        ((pRegExpr (compileRegex "<!DOCTYPE\\s+") >>= withAttribute "Doctype") >>~ pushContext "Doctype")
                        <|>
                        ((pRegExpr (compileRegex "<\\?[\\w:-]*") >>= withAttribute "Processing Instruction") >>~ pushContext "PI")
                        <|>
                        ((pRegExpr (compileRegex "<style\\b") >>= withAttribute "Element") >>~ pushContext "CSS")
                        <|>
                        ((pRegExpr (compileRegex "<script\\b") >>= withAttribute "Element") >>~ pushContext "JS")
                        <|>
                        ((pRegExpr (compileRegex "<pre\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<div\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<table\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<ul\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<ol\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<dl\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "</pre\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</div\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</table\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</ul\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</ol\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</dl\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((parseRules "FindDTDRules"))
                        <|>
                        ((parseRules "FindEntityRefs")))
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

parseRules "FindAttributes" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Attribute"))
                        <|>
                        ((pRegExpr (compileRegex "\\s+[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Attribute"))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Attribute") >>~ pushContext "Value"))
     return (attr, result)

parseRules "FindDTDRules" = 
  do (attr, result) <- ((pRegExpr (compileRegex "<!(ELEMENT|ENTITY|ATTLIST|NOTATION)\\b") >>= withAttribute "Doctype") >>~ pushContext "Doctype Markupdecl")
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment"))
                        <|>
                        ((pString False "-->" >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "-(-(?!->))+") >>= withAttribute "Error")))
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
                        ((parseRules "FindDTDRules"))
                        <|>
                        ((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pRegExpr (compileRegex "<\\?[\\w:-]*") >>= withAttribute "Processing Instruction") >>~ pushContext "PI")
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

parseRules "El Open" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindAttributes"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "El Close" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "El Close 2" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "El Close 3" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "CSS" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ pushContext "CSS content")
                        <|>
                        ((parseRules "FindAttributes"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "CSS content" = 
  do (attr, result) <- (((pRegExpr (compileRegex "</style\\b") >>= withAttribute "Element") >>~ pushContext "El Close 2")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Css.parseExpression)))
     return (attr, result)

parseRules "JS" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ pushContext "JS content")
                        <|>
                        ((parseRules "FindAttributes"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "JS content" = 
  do (attr, result) <- (((pRegExpr (compileRegex "</script\\b") >>= withAttribute "Element") >>~ pushContext "El Close 2")
                        <|>
                        ((pRegExpr (compileRegex "//(?=.*</script\\b)") >>= withAttribute "Comment") >>~ pushContext "JS comment close")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Javascript.parseExpression)))
     return (attr, result)

parseRules "JS comment close" = 
  do (attr, result) <- (((pRegExpr (compileRegex "</script\\b") >>= withAttribute "Element") >>~ pushContext "El Close 3")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Value" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Value") >>~ pushContext "Value DQ")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Value") >>~ pushContext "Value SQ")
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        (pushContext "Value NQ" >> return ([], "")))
     return (attr, result)

parseRules "Value NQ" = 
  do (attr, result) <- (((parseRules "FindEntityRefs"))
                        <|>
                        ((pRegExpr (compileRegex "/(?!>)") >>= withAttribute "Value"))
                        <|>
                        ((pRegExpr (compileRegex "[^/><\"'\\s]") >>= withAttribute "Value"))
                        <|>
                        ((popContext >> popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Value DQ" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Value") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindEntityRefs")))
     return (attr, result)

parseRules "Value SQ" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "Value") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindEntityRefs")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
