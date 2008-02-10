{- This module was generated from data in the Kate syntax highlighting file makefile.xml, version 1.09,
   by  Per Wigren (wigren@home.se) -}

module Text.Highlighting.Kate.Syntax.Makefile ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Makefile"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "GNUmakefile;Makefile*;makefile*"

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
  setState $ st { synStLanguage = "Makefile" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Makefile",["Normal"])], synStLanguage = "Makefile", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String" -> (popContext >> return ())
    "Value" -> return ()
    "VarFromValue" -> return ()
    "VarFromNormal" -> return ()
    "Commands" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Comment","Comment"),("String","String"),("Variable","DataType"),("Target","DecVal"),("Section","Others"),("Operator","Char"),("Commands","BaseN"),("Special","Float")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String","String"),("Value","String"),("VarFromValue","Variable"),("VarFromNormal","Variable"),("Commands","Normal Text")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["include","define","else","endef","endif","ifdef","ifeq","ifndef","ifneq"] >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "[_\\w\\d]*\\s*(?=:=|=)") >>= withAttribute "Variable") >>~ pushContext "Value")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "[_\\w\\d-]*\\s*:") >>= withAttribute "Target"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "[.].*:") >>= withAttribute "Section"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pRegExpr (compileRegex "[$][\\({]") >>= withAttribute "Operator") >>~ pushContext "VarFromNormal")
                        <|>
                        ((pDetect2Chars False '\\' '#' >>= withAttribute "Special"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "Special"))
                        <|>
                        ((pAnyChar "+*=%$():\\;" >>= withAttribute "Operator"))
                        <|>
                        ((pFirstNonSpace >> pAnyChar "@-" >>= withAttribute "Operator") >>~ pushContext "Commands")
                        <|>
                        ((pRegExpr (compileRegex "#.*$") >>= withAttribute "Comment")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Value" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\$") >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "[^\\\\]?$") >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[$][\\({]") >>= withAttribute "Operator") >>~ pushContext "VarFromValue")
                        <|>
                        ((pRegExpr (compileRegex "@[-_\\d\\w]*@") >>= withAttribute "Special") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False ';' >>= withAttribute "Operator") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "VarFromValue" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[\\)}](?=/)") >>= withAttribute "Operator") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[\\)}][^$]") >>= withAttribute "Operator") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[\\)}]$") >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "VarFromNormal" = 
  do (attr, result) <- ((pAnyChar ")}" >>= withAttribute "Operator") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Commands" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[$][\\({]") >>= withAttribute "Operator") >>~ pushContext "VarFromNormal")
                        <|>
                        ((pRegExpr (compileRegex "[_\\w-]*\\b") >>= withAttribute "Commands") >>~ (popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
