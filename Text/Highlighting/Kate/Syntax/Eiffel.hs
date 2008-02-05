{- This module was generated from data in the Kate syntax highlighting file eiffel.xml, version 1.02,
   by  Sebastian Vuorinen -}

module Text.Highlighting.Kate.Syntax.Eiffel ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Eiffel"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.e"

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
  setState $ st { synStLanguage = "Eiffel" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Eiffel",["Normal"])], synStLanguage = "Eiffel", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Quoted String" -> (popContext >> return ())
    "Documentation" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Predefined entities","Others"),("Assertions","Others"),("Decimal","DecVal"),("Float","Float"),("Char","Char"),("String","String"),("Comment","Comment")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("Quoted String","String"),("Documentation","Comment")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword ["agent","alias","all","and","as","assign","class","convert","create","creation","debug","deferred","do","else","elseif","end","expanded","export","external","feature","from","frozen","if","implies","indexing","infix","inherit","inspect","is","like","local","loop","not","obsolete","old","once","or","prefix","pure","redefine","reference","rename","rescue","retry","separate","then","undefine"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["Current","False","Precursor","Result","True","TUPLE"] >>= withAttribute "Predefined entities"))
                        <|>
                        ((pKeyword ["check","ensure","require","variant","invariant"] >>= withAttribute "Assertions"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pHlCChar >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "Quoted String")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "Documentation"))
     return (attr, result)

parseRules "Quoted String" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Documentation" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x