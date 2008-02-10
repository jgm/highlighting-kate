{- This module was generated from data in the Kate syntax highlighting file json.xml, version 1.00,
   by  Sebastian Pipping (webmaster@hartwork.org) -}

module Text.Highlighting.Kate.Syntax.Json ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "JSON"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.json"

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
  setState $ st { synStLanguage = "JSON" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("JSON",["Normal"])], synStLanguage = "JSON", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Pair" -> return ()
    "String_Key" -> return ()
    "Value" -> return ()
    "String_Value" -> return ()
    "Array" -> return ()
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

styles = [("Style_Normal","Normal"),("Style_Seperator_Pair","Normal"),("Style_Seperator_Array","Normal"),("Style_Decimal","DecVal"),("Style_Float","Float"),("Style_String_Key","DataType"),("Style_String_Value","String"),("Style_String_Key_Char","DataType"),("Style_String_Value_Char","String"),("Style_Keyword","DecVal"),("Style_Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Style_Error"),("Pair","Style_Error"),("String_Key","Style_String_Key"),("Value","Style_Error"),("String_Value","Style_String_Value"),("Array","Style_Error")]

parseRules "Normal" = 
  do (attr, result) <- ((pDetectChar False '{' >>= withAttribute "Style_Seperator_Pair") >>~ pushContext "Pair")
     return (attr, result)

parseRules "Pair" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Style_String_Key") >>~ pushContext "String_Key")
                        <|>
                        ((pDetectChar False ':' >>= withAttribute "Style_Seperator_Pair") >>~ pushContext "Value")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Style_Seperator_Pair") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False ',' >>= withAttribute "Style_Seperator_Pair"))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Style_Normal")))
     return (attr, result)

parseRules "String_Key" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Style_String_Key") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\\\(?:[\"\\\\/bfnrt]|u[0-9a-fA-f]{4})") >>= withAttribute "Style_String_Key_Char")))
     return (attr, result)

parseRules "Value" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Style_String_Value") >>~ pushContext "String_Value")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Style_Seperator_Pair") >>~ pushContext "Pair")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Style_Seperator_Array") >>~ pushContext "Array")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Style_Error") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False ',' >>= withAttribute "Style_Error") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Style_Normal"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["null","true","false"] >>= withAttribute "Style_Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "-?(?:[0-9]|[1-9][0-9]+)\\.[0-9]+(?:[eE][+-]?[0-9]+)?") >>= withAttribute "Style_Float"))
                        <|>
                        ((pRegExpr (compileRegex "-?(?:[0-9]|[1-9][0-9]+)(?:[eE][+-]?[0-9]+)?") >>= withAttribute "Style_Decimal")))
     return (attr, result)

parseRules "String_Value" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Style_String_Value") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\\\(?:[\"\\\\/bfnrt]|u[0-9a-fA-f]{4})") >>= withAttribute "Style_String_Value_Char")))
     return (attr, result)

parseRules "Array" = 
  do (attr, result) <- (((pDetectChar False ',' >>= withAttribute "Style_Seperator_Array"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Style_Seperator_Array") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Style_Seperator_Pair") >>~ pushContext "Pair")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Style_String_Value") >>~ pushContext "String_Value")
                        <|>
                        ((pDetectSpaces >>= withAttribute "Style_Normal"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["null","true","false"] >>= withAttribute "Style_Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "-?(?:[0-9]|[1-9][0-9]+)\\.[0-9]+(?:[eE][+-]?[0-9]+)?") >>= withAttribute "Style_Float"))
                        <|>
                        ((pRegExpr (compileRegex "-?(?:[0-9]|[1-9][0-9]+)(?:[eE][+-]?[0-9]+)?") >>= withAttribute "Style_Decimal")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
