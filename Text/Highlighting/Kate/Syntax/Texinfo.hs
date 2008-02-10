{- This module was generated from data in the Kate syntax highlighting file texinfo.xml, version 0.2,
   by  Daniel Franke (franke.daniel@gmail.com) -}

module Text.Highlighting.Kate.Syntax.Texinfo ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Texinfo"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.texi"

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
  setState $ st { synStLanguage = "Texinfo" }
  context <- currentContext <|> (pushContext "Normal Text" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Texinfo",["Normal Text"])], synStLanguage = "Texinfo", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal Text" -> return ()
    "singleLineComment" -> (popContext >> return ())
    "multiLineComment" -> return ()
    "nodeFolding" -> return ()
    "folding" -> return ()
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

styles = [("Normal Text","Normal"),("Comment","Comment"),("Command","Function")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("singleLineComment","Comment"),("multiLineComment","Comment"),("nodeFolding","Normal Text"),("folding","Normal Text")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pRegExpr (compileRegex "@c(omment)?\\b") >>= withAttribute "Comment") >>~ pushContext "singleLineComment")
                        <|>
                        ((pRegExpr (compileRegex "@ignore\\b") >>= withAttribute "Comment") >>~ pushContext "multiLineComment")
                        <|>
                        ((pRegExpr (compileRegex "@node\\b") >>= withAttribute "Command") >>~ pushContext "nodeFolding")
                        <|>
                        ((pRegExpr (compileRegex "@(menu|smallexample|table|multitable)\\b") >>= withAttribute "Command") >>~ pushContext "folding")
                        <|>
                        ((pRegExpr (compileRegex "@[\\w]+(\\{([\\w]+[\\s]*)+\\})?") >>= withAttribute "Command")))
     return (attr, result)

parseRules "singleLineComment" = 
  do (attr, result) <- ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
     return (attr, result)

parseRules "multiLineComment" = 
  do (attr, result) <- (((pString False "@end ignore" >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "nodeFolding" = 
  do (attr, result) <- (((pRegExpr (compileRegex "@node\\b") >>= withAttribute "Command") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Normal Text")))
     return (attr, result)

parseRules "folding" = 
  do (attr, result) <- (((pRegExpr (compileRegex "@end (menu|smallexample|table|multitable)\\b") >>= withAttribute "Command") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Normal Text")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
