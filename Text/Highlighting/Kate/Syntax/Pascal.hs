{- This module was generated from data in the Kate syntax highlighting file pascal.xml, version 1.21,
   by   -}

module Text.Highlighting.Kate.Syntax.Pascal ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Pascal"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.pp;*.pas;*.p"

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
  setState $ st { synStLanguage = "Pascal" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Pascal",["Normal"])], synStLanguage = "Pascal", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String" -> (popContext >> return ())
    "Prep1" -> (popContext >> return ())
    "Prep2" -> (popContext >> return ())
    "Comment1" -> return ()
    "Comment2" -> return ()
    "Comment3" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("ISO/Delphi Extended","Keyword"),("Type","DataType"),("Number","DecVal"),("String","String"),("Directive","Others"),("Comment","Comment"),("Alert","Alert")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String","String"),("Prep1","Directive"),("Prep2","Directive"),("Comment1","Comment"),("Comment2","Comment"),("Comment3","Comment")]

parseRules "Normal" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\b(begin|case|record)(?=(\\{[^}]*(\\}|$)|\\(\\*.*(\\*\\)|$))*([\\s]|$|//))") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\b((object|class)(?=(\\(.*\\))?(\\{[^}]*(\\}|$)|\\(\\*.*(\\*\\)|$))*;?([\\s]|$|//))|try(?=(\\{[^}]*(\\}|$)|\\(\\*.*(\\*\\)|$))*([\\s]|$|//)))") >>= withAttribute "ISO/Delphi Extended"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend(?=((\\{[^}]*(\\}|$)|\\(\\*.*(\\*\\)|$))*)([.;\\s]|$)|//|$)") >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["and","array","asm","case","const","div","do","downto","else","file","for","function","goto","if","in","label","mod","nil","not","of","operator","or","packed","procedure","program","record","repeat","set","then","to","type","unit","until","uses","var","while","with","xor","at","automated","break","continue","dispinterface","dispose","exit","false","finalization","initialization","library","new","published","resourcestring","self","true"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["abstract","as","bindable","constructor","destructor","except","export","finally","import","implementation","inherited","inline","interface","is","module","on","only","otherwise","override","private","property","protected","public","read","qualified","raise","restricted","shl","shr","threadvar","try","virtual","write"] >>= withAttribute "ISO/Delphi Extended"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["Integer","Cardinal","ShortInt","SmallInt","LongInt","Int64","Byte","Word","LongWord","Char","AnsiChar","WideChar","Boolean","ByteBool","WordBool","LongBool","Single","Double","Extended","Comp","Currency","Real","Real48","String","ShortString","AnsiString","WideString","Pointer","Variant","File","Text"] >>= withAttribute "Type"))
                        <|>
                        ((pFloat >>= withAttribute "Number"))
                        <|>
                        ((pInt >>= withAttribute "Number"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pString False "(*$" >>= withAttribute "Directive") >>~ pushContext "Prep1")
                        <|>
                        ((pDetect2Chars False '{' '$' >>= withAttribute "Directive") >>~ pushContext "Prep2")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Comment") >>~ pushContext "Comment1")
                        <|>
                        ((pDetect2Chars False '(' '*' >>= withAttribute "Comment") >>~ pushContext "Comment2")
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Comment3"))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- ((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Prep1" = 
  do (attr, result) <- ((pDetect2Chars False '*' ')' >>= withAttribute "Directive") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Prep2" = 
  do (attr, result) <- ((pDetectChar False '}' >>= withAttribute "Directive") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Comment1" = 
  do (attr, result) <- (((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["FIXME","TODO","###"] >>= withAttribute "Alert"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Comment2" = 
  do (attr, result) <- (((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["FIXME","TODO","###"] >>= withAttribute "Alert"))
                        <|>
                        ((pDetect2Chars False '*' ')' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Comment3" = 
  do (attr, result) <- ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["FIXME","TODO","###"] >>= withAttribute "Alert"))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
