{- This module was generated from data in the Kate syntax highlighting file matlab.xml, version 1.20,
   by   -}

module Text.Highlighting.Kate.Syntax.Matlab ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Matlab"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.m;*.M"

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
  setState $ st { synStLanguage = "Matlab" }
  context <- currentContext <|> (pushContext "_normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Matlab",["_normal"])], synStLanguage = "Matlab", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "_normal" -> (popContext >> return ())
    "_adjoint" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Variable","Normal"),("Operator","Normal"),("Number","Float"),("Delimiter","Normal"),("String","String"),("System","BaseN"),("Incomplete String","Char"),("Keyword","Normal"),("Comment","Comment")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("_normal","Normal Text"),("_adjoint","Operator")]

parseRules "_normal" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[a-zA-Z]\\w*(?=')") >>= withAttribute "Variable") >>~ pushContext "_adjoint")
                        <|>
                        ((pRegExpr (compileRegex "(\\d+(\\.\\d+)?|\\.\\d+)([eE][+-]?\\d+)?[ij]?(?=')") >>= withAttribute "Number") >>~ pushContext "_adjoint")
                        <|>
                        ((pRegExpr (compileRegex "[\\)\\]}](?=')") >>= withAttribute "Delimiter") >>~ pushContext "_adjoint")
                        <|>
                        ((pRegExpr (compileRegex "\\.'(?=')") >>= withAttribute "Operator") >>~ pushContext "_adjoint")
                        <|>
                        ((pRegExpr (compileRegex "'[^']*(''[^']*)*'(?=[^']|$)") >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*(''[^']*)*") >>= withAttribute "Incomplete String"))
                        <|>
                        ((pKeyword ["break","case","catch","continue","else","elseif","end","for","function","global","if","otherwise","persistent","return","switch","try","while"] >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "%.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "!.*$") >>= withAttribute "System"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]\\w*") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "(\\d+(\\.\\d+)?|\\.\\d+)([eE][+-]?\\d+)?[ij]?") >>= withAttribute "Number"))
                        <|>
                        ((pAnyChar "()[]{}" >>= withAttribute "Delimiter"))
                        <|>
                        ((pString False "..." >>= withAttribute "Operator"))
                        <|>
                        ((pString False "==" >>= withAttribute "Operator"))
                        <|>
                        ((pString False "~=" >>= withAttribute "Operator"))
                        <|>
                        ((pString False "<=" >>= withAttribute "Operator"))
                        <|>
                        ((pString False ">=" >>= withAttribute "Operator"))
                        <|>
                        ((pString False "&&" >>= withAttribute "Operator"))
                        <|>
                        ((pString False "||" >>= withAttribute "Operator"))
                        <|>
                        ((pString False ".*" >>= withAttribute "Operator"))
                        <|>
                        ((pString False ".^" >>= withAttribute "Operator"))
                        <|>
                        ((pString False "./" >>= withAttribute "Operator"))
                        <|>
                        ((pString False ".'" >>= withAttribute "Operator"))
                        <|>
                        ((pAnyChar "*+-/\\&|<>~^=,;:@" >>= withAttribute "Operator")))
     return (attr, result)

parseRules "_adjoint" = 
  do (attr, result) <- ((pRegExpr (compileRegex "'+") >>= withAttribute "Operator") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x