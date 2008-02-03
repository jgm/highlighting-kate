{- This module was generated from data in the Kate syntax highlighting file c.xml, version 1.41,
   by   -}

module Text.Highlighting.Kate.Syntax.C ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Doxygen
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "C"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.c;*.C;*.h"

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
  setState $ st { synStLanguage = "C" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("C",["Normal"])], synStLanguage = "C", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String" -> (popContext >> return ())
    "Region Marker" -> (popContext >> return ())
    "Commentar 1" -> (popContext >> return ())
    "Commentar 2" -> return ()
    "AfterHash" -> (popContext >> return ())
    "Preprocessor" -> (popContext >> return ())
    "Define" -> (popContext >> return ())
    "Commentar/Preprocessor" -> return ()
    "Outscoped" -> return ()
    "Outscoped intern" -> return ()
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Data Type","DataType"),("Decimal","DecVal"),("Octal","BaseN"),("Hex","BaseN"),("Float","Float"),("Char","Char"),("String","String"),("String Char","Char"),("Comment","Comment"),("Symbol","Normal"),("Preprocessor","Others"),("Prep. Lib","Others"),("Alert","Alert"),("Region Marker","RegionMarker"),("Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String","String"),("Region Marker","Region Marker"),("Commentar 1","Comment"),("Commentar 2","Comment"),("AfterHash","Error"),("Preprocessor","Preprocessor"),("Define","Preprocessor"),("Commentar/Preprocessor","Comment"),("Outscoped","Comment"),("Outscoped intern","Comment")]

parseRules "Normal" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*if\\s+0") >>= withAttribute "Preprocessor") >>~ pushContext "Outscoped")
                        <|>
                        ((pFirstNonSpace >> pDetectChar False '#' >>= withAttribute "Normal Text") >>~ pushContext "AfterHash")
                        <|>
                        ((pFirstNonSpace >> pString False "//BEGIN" >>= withAttribute "Region Marker") >>~ pushContext "Region Marker")
                        <|>
                        ((pFirstNonSpace >> pString False "//END" >>= withAttribute "Region Marker") >>~ pushContext "Region Marker")
                        <|>
                        ((pKeyword ["break","case","continue","default","do","else","enum","extern","for","goto","if","inline","return","sizeof","struct","switch","typedef","union","while"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["auto","char","const","double","float","int","long","register","restrict","short","signed","static","unsigned","void","volatile","_Imaginary","_Complex","_Bool"] >>= withAttribute "Data Type"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Symbol"))
                        <|>
                        (withChildren (pFloat >>= withAttribute "Float") ((pAnyChar "fF" >>= withAttribute "Float")))
                        <|>
                        ((pHlCOct >>= withAttribute "Octal"))
                        <|>
                        ((pHlCHex >>= withAttribute "Hex"))
                        <|>
                        (withChildren (pInt >>= withAttribute "Decimal") (((pString False "ULL" >>= withAttribute "Decimal"))
                                                                          <|>
                                                                          ((pString False "LUL" >>= withAttribute "Decimal"))
                                                                          <|>
                                                                          ((pString False "LLU" >>= withAttribute "Decimal"))
                                                                          <|>
                                                                          ((pString False "UL" >>= withAttribute "Decimal"))
                                                                          <|>
                                                                          ((pString False "LU" >>= withAttribute "Decimal"))
                                                                          <|>
                                                                          ((pString False "LL" >>= withAttribute "Decimal"))
                                                                          <|>
                                                                          ((pString False "U" >>= withAttribute "Decimal"))
                                                                          <|>
                                                                          ((pString False "L" >>= withAttribute "Decimal"))))
                        <|>
                        ((pHlCChar >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Doxygen.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Commentar 1")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Commentar 2")
                        <|>
                        ((pAnyChar ":!%&()+,-/.*<=>?[]|~^;" >>= withAttribute "Symbol")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "String"))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Region Marker" = 
  pzero

parseRules "Commentar 1" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Commentar 2" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "AfterHash" = 
  do (attr, result) <- (((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*if(?:def|ndef)?(?=\\s+\\S)") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*endif") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*define.*((?=\\\\))") >>= withAttribute "Preprocessor") >>~ pushContext "Define")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*(?:el(?:se|if)|include(?:_next)?|define|undef|line|error|warning|pragma)") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s+[0-9]+") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor"))
     return (attr, result)

parseRules "Preprocessor" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Preprocessor"))
                        <|>
                        ((pRangeDetect '"' '"' >>= withAttribute "Prep. Lib"))
                        <|>
                        ((pRangeDetect '<' '>' >>= withAttribute "Prep. Lib"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Doxygen.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Commentar/Preprocessor"))
     return (attr, result)

parseRules "Define" = 
  do (attr, result) <- ((pLineContinue >>= withAttribute "Preprocessor"))
     return (attr, result)

parseRules "Commentar/Preprocessor" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Outscoped" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Doxygen.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Commentar 1")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Commentar 2")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*if") >>= withAttribute "Comment") >>~ pushContext "Outscoped intern")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*el(?:se|if)") >>= withAttribute "Preprocessor") >>~ (popContext >> return ()))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*endif") >>= withAttribute "Preprocessor") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Outscoped intern" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Doxygen.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Commentar 1")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Commentar 2")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*if") >>= withAttribute "Comment") >>~ pushContext "Outscoped intern")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*endif") >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x