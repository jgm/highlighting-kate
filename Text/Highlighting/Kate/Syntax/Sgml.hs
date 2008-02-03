{- This module was generated from data in the Kate syntax highlighting file sgml.xml, version 1.02,
   by   -}

module Text.Highlighting.Kate.Syntax.Sgml ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "SGML"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.sgml"

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
  setState $ st { synStLanguage = "SGML" }
  context <- currentContext <|> (pushContext "Normal Text" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("SGML",["Normal Text"])], synStLanguage = "SGML", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal Text" -> return ()
    "Attribute" -> return ()
    "Value" -> return ()
    "Value 2" -> return ()
    "Comment" -> return ()
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

styles = [("Normal Text","Normal"),("Tag","Keyword"),("Attribute Name","Others"),("Attribute Value","DataType"),("Comment","Comment")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("Attribute","Attribute Name"),("Value","Attribute Value"),("Value 2","Attribute Value"),("Comment","Comment")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pRegExpr (compileRegex "<\\s*\\/?\\s*[a-zA-Z_:][a-zA-Z0-9._:-]*") >>= withAttribute "Tag") >>~ pushContext "Attribute"))
     return (attr, result)

parseRules "Attribute" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*=\\s*") >>= withAttribute "Normal Text") >>~ pushContext "Value"))
     return (attr, result)

parseRules "Value" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Tag") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Tag") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Attribute Value") >>~ pushContext "Value 2"))
     return (attr, result)

parseRules "Value 2" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "Attribute Value") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- ((pString False "-->" >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x