{- This module was generated from data in the Kate syntax highlighting file relaxngcompact.xml, version 0.1,
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
    "string" -> return ()
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

styles = [("Normal Text","Normal"),("Comments","Comment"),("String","String"),("Keyword","Keyword")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("Comments","Comments"),("string","String")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pFirstNonSpace >> pDetect2Chars False '#' '#' >>= withAttribute "Comments") >>~ pushContext "Comments")
                        <|>
                        ((pKeyword " \n\t.():!+,<=>%&*/;?[]^{|}~\\" ["attribute","div","element","empty","external","grammar","include","inherit","list","mixed","notAllowed","parent","start","string","text","token"] >>= withAttribute "Keyword"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "string"))
     return (attr, result)

parseRules "Comments" = 
  pzero

parseRules "string" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
