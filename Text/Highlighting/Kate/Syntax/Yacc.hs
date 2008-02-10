{- This module was generated from data in the Kate syntax highlighting file yacc.xml, version 1.03,
   by  Jan Villat (jan.villat@net2000.ch) -}

module Text.Highlighting.Kate.Syntax.Yacc ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Cpp
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Yacc/Bison"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.y"

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
  setState $ st { synStLanguage = "Yacc/Bison" }
  context <- currentContext <|> (pushContext "Pre Start" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Yacc/Bison",["Pre Start"])], synStLanguage = "Yacc/Bison", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Pre Start" -> return ()
    "C Declarations" -> return ()
    "Declarations" -> return ()
    "Union Start" -> return ()
    "Union In" -> return ()
    "Union InIn" -> return ()
    "Rules" -> return ()
    "Rule In" -> return ()
    "User Code" -> return ()
    "Percent Command" -> (popContext >> return ())
    "Percent Command In" -> (popContext >> popContext >> return ())
    "PC type" -> (popContext >> popContext >> popContext >> return ())
    "Comment" -> return ()
    "CommentStar" -> return ()
    "CommentSlash" -> return ()
    "StringOrChar" -> return ()
    "String" -> (popContext >> return ())
    "Char" -> (popContext >> return ())
    "Normal C Bloc" -> return ()
    "Dol" -> return ()
    "DolEnd" -> return ()
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

styles = [("Normal Text","Normal"),("Definition","Normal"),("Comment","Comment"),("Content-Type Delimiter","BaseN"),("Directive","Keyword"),("Rule","String"),("Backslash Code","String"),("Alert","Alert"),("String","String"),("String Char","Char"),("Data Type","DataType")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Pre Start","Normal Text"),("C Declarations","Normal Text"),("Declarations","Normal Text"),("Union Start","Normal Text"),("Union In","Normal Text"),("Union InIn","Normal Text"),("Rules","Rule"),("Rule In","Definition"),("User Code","Normal Text"),("Percent Command","Directive"),("Percent Command In","NormalText"),("PC type","Data Type"),("Comment","Comment"),("CommentStar","Comment"),("CommentSlash","Comment"),("StringOrChar","NormalText"),("String","String"),("Char","String Char"),("Normal C Bloc","Normal Text"),("Dol","Normal Text"),("DolEnd","Normal Text")]

parseRules "Pre Start" = 
  do (attr, result) <- (((parseRules "Comment"))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pColumn 0 >> pDetect2Chars False '%' '{' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "C Declarations")
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "Normal Text") >>~ pushContext "Declarations"))
     return (attr, result)

parseRules "C Declarations" = 
  do (attr, result) <- (((parseRules "Comment"))
                        <|>
                        ((pColumn 0 >> pDetect2Chars False '%' '}' >>= withAttribute "Content-Type Delimiter") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Declarations" = 
  do (attr, result) <- (((parseRules "Comment"))
                        <|>
                        ((pString False "%union" >>= withAttribute "Directive") >>~ pushContext "Union Start")
                        <|>
                        ((pDetect2Chars False '%' '%' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "Rules")
                        <|>
                        ((pColumn 0 >> pDetect2Chars False '%' '{' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "C Declarations")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Directive") >>~ pushContext "Percent Command"))
     return (attr, result)

parseRules "Union Start" = 
  do (attr, result) <- (((parseRules "Comment"))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Union In")
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "Alert") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Union In" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Union InIn")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Union InIn" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Union InIn")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Rules" = 
  do (attr, result) <- (((parseRules "Comment"))
                        <|>
                        ((pDetect2Chars False '%' '%' >>= withAttribute "Content-Type Delimiter") >>~ pushContext "User Code")
                        <|>
                        ((pDetectChar False ':' >>= withAttribute "Normal Text") >>~ pushContext "Rule In"))
     return (attr, result)

parseRules "Rule In" = 
  do (attr, result) <- (((parseRules "Comment"))
                        <|>
                        ((pDetectChar False ';' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Normal C Bloc")
                        <|>
                        ((pDetectChar False '|' >>= withAttribute "Normal Text"))
                        <|>
                        ((parseRules "StringOrChar")))
     return (attr, result)

parseRules "User Code" = 
  do (attr, result) <- ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd)))
     return (attr, result)

parseRules "Percent Command" = 
  do (attr, result) <- (((parseRules "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\W") >>= withAttribute "Normal Text") >>~ pushContext "Percent Command In"))
     return (attr, result)

parseRules "Percent Command In" = 
  do (attr, result) <- (((parseRules "StringOrChar"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Data Type") >>~ pushContext "PC type"))
     return (attr, result)

parseRules "PC type" = 
  do (attr, result) <- ((pDetectChar False '>' >>= withAttribute "Data Type") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "CommentStar")
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "CommentSlash"))
     return (attr, result)

parseRules "CommentStar" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "CommentSlash" = 
  do (attr, result) <- ((pRegExpr (compileRegex "[^\\\\]$") >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "StringOrChar" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "String Char") >>~ pushContext "Char")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String"))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Backslash Code"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Char" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Backslash Code"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String Char") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Normal C Bloc" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Normal C Bloc")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Cpp.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Directive") >>~ pushContext "Dol"))
     return (attr, result)

parseRules "Dol" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<[^>]+>") >>= withAttribute "Data Type") >>~ pushContext "DolEnd")
                        <|>
                        (pushContext "DolEnd" >> return ([], "")))
     return (attr, result)

parseRules "DolEnd" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\d+") >>= withAttribute "Directive") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Directive") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
