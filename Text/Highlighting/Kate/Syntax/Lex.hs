{- This module was generated from data in the Kate syntax highlighting file lex.xml, version 1.01,
   by  Jan Villat (jan.villat@net2000.ch) -}

module Text.Highlighting.Kate.Syntax.Lex ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Cpp
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Lex/Flex"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.l;*.lex;*.flex"

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
  setState $ st { synStLanguage = "Lex/Flex" }
  context <- currentContext <|> (pushContext "Pre Start" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Lex/Flex",["Pre Start"])], synStLanguage = "Lex/Flex", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Pre Start" -> return ()
    "Definitions" -> return ()
    "Rules" -> return ()
    "User Code" -> return ()
    "Percent Command" -> (popContext >> return ())
    "Comment" -> return ()
    "Definition RegExpr" -> (popContext >> return ())
    "Rule RegExpr" -> (popContext >> return ())
    "RegExpr (" -> return ()
    "RegExpr [" -> return ()
    "RegExpr {" -> return ()
    "RegExpr Q" -> return ()
    "RegExpr Base" -> return ()
    "Start Conditions Scope" -> return ()
    "Action" -> (popContext >> return ())
    "Detect C" -> return ()
    "Indented C" -> (popContext >> return ())
    "Lex C Bloc" -> return ()
    "Lex Rule C Bloc" -> return ()
    "Normal C Bloc" -> return ()
    "Action C" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Definition","DataType"),("Comment","Comment"),("Content-Type Delimiter","BaseN"),("Directive","Keyword"),("RegExpr","String"),("Backslash Code","String"),("Alert","Alert")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Pre Start","Normal Text"),("Definitions","Normal Text"),("Rules","Normal Text"),("User Code","Normal Text"),("Percent Command","Directive"),("Comment","Comment"),("Definition RegExpr","RegExpr"),("Rule RegExpr","RegExpr"),("RegExpr (","RegExpr"),("RegExpr [","RegExpr"),("RegExpr {","RegExpr"),("RegExpr Q","RegExpr"),("RegExpr Base","RegExpr"),("Start Conditions Scope","Normal Text"),("Action","Normal Text"),("Detect C","Normal Text"),("Indented C","Normal Text"),("Lex C Bloc","Normal Text"),("Lex Rule C Bloc","Normal Text"),("Normal C Bloc","Normal Text"),("Action C","Normal Text")]

parseRules "Pre Start" = 
  do (attr, result) <- ((pRegExpr (compileRegex ".") >>= withAttribute "Normal Text") >>~ pushContext "Definitions")
     return (attr, result)

parseRules "Definitions" = 
  do (attr, result) <- (((parseRules "Detect C"))
                        <|>
                        ((pDetect2Chars False '%' '%' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "Rules")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Directive") >>~ pushContext "Percent Command")
                        <|>
                        ((pColumn 0 >> pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "[A-Za-z_]\\w*\\s+") >>= withAttribute "Definition") >>~ pushContext "Definition RegExpr"))
     return (attr, result)

parseRules "Rules" = 
  do (attr, result) <- (((parseRules "Detect C"))
                        <|>
                        ((pDetect2Chars False '%' '%' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "User Code")
                        <|>
                        (pushContext "Rule RegExpr" >> return ([], "")))
     return (attr, result)

parseRules "User Code" = 
  do (attr, result) <- ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd)))
     return (attr, result)

parseRules "Percent Command" = 
  pzero

parseRules "Comment" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Definition RegExpr" = 
  do (attr, result) <- (((parseRules "RegExpr Base"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "RegExpr"))
                        <|>
                        ((pRegExpr (compileRegex ".*") >>= withAttribute "Alert")))
     return (attr, result)

parseRules "Rule RegExpr" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\{$") >>= withAttribute "Content-Type Delimiter") >>~ pushContext "Start Conditions Scope")
                        <|>
                        ((parseRules "RegExpr Base"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "RegExpr"))
                        <|>
                        ((pRegExpr (compileRegex "\\s+") >>= withAttribute "Normal Text") >>~ pushContext "Action"))
     return (attr, result)

parseRules "RegExpr (" = 
  do (attr, result) <- (((parseRules "RegExpr Base"))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "RegExpr") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "RegExpr")))
     return (attr, result)

parseRules "RegExpr [" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Backslash Code"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "RegExpr") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "RegExpr")))
     return (attr, result)

parseRules "RegExpr {" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Backslash Code"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "RegExpr") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "RegExpr")))
     return (attr, result)

parseRules "RegExpr Q" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Backslash Code"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "RegExpr") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "RegExpr")))
     return (attr, result)

parseRules "RegExpr Base" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Backslash Code"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "RegExpr") >>~ pushContext "RegExpr (")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "RegExpr") >>~ pushContext "RegExpr [")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "RegExpr") >>~ pushContext "RegExpr {")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "RegExpr") >>~ pushContext "RegExpr Q"))
     return (attr, result)

parseRules "Start Conditions Scope" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*\\}") >>= withAttribute "Content-Type Delimiter") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*") >>= withAttribute "Normal Text") >>~ pushContext "Rule RegExpr")
                        <|>
                        (pushContext "Rule RegExpr" >> return ([], "")))
     return (attr, result)

parseRules "Action" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\|\\s*$") >>= withAttribute "Directive"))
                        <|>
                        ((pDetect2Chars False '%' '{' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "Lex Rule C Bloc")
                        <|>
                        (pushContext "Action C" >> return ([], "")))
     return (attr, result)

parseRules "Detect C" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "\\s") >>= withAttribute "Normal Text") >>~ pushContext "Indented C")
                        <|>
                        ((pColumn 0 >> pDetect2Chars False '%' '{' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "Lex C Bloc"))
     return (attr, result)

parseRules "Indented C" = 
  do (attr, result) <- ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd)))
     return (attr, result)

parseRules "Lex C Bloc" = 
  do (attr, result) <- (((pColumn 0 >> pDetect2Chars False '%' '}' >>= withAttribute "Content-Type Delimiter") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Lex Rule C Bloc" = 
  do (attr, result) <- (((pDetect2Chars False '%' '}' >>= withAttribute "Content-Type Delimiter") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Normal C Bloc" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Normal C Bloc")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Action C" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Normal C Bloc")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Alert"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x