{- This module was generated from data in the Kate syntax highlighting file objectivec.xml, version 1.07,
   by   -}

module Text.Highlighting.Kate.Syntax.Objectivec ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Doxygen
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Objective-C"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.m;*.h"

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
  setState $ st { synStLanguage = "Objective-C" }
  context <- currentContext <|> (pushContext "Default" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Objective-C",["Default"])], synStLanguage = "Objective-C", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Default" -> return ()
    "String" -> (popContext >> return ())
    "SingleLineComment" -> (popContext >> return ())
    "MultiLineComment" -> return ()
    "Preprocessor" -> pushContext "Default"
    "MultiLineCommentPrep" -> return ()
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Data Type","DataType"),("Decimal","DecVal"),("Octal","BaseN"),("Hex","BaseN"),("Float","Float"),("Char","Char"),("String","String"),("String Char","Char"),("Comment","Comment"),("Symbol","Normal"),("Preprocessor","Others"),("Prep. Lib","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Default","Normal Text"),("String","String"),("SingleLineComment","Comment"),("MultiLineComment","Comment"),("Preprocessor","Preprocessor"),("MultiLineCommentPrep","Comment")]

parseRules "Default" = 
  do (attr, result) <- (((pKeyword ["break","case","continue","default","do","else","enum","extern","for","goto","if","return","sizeof","struct","switch","typedef","union","while","@class","@defs","@encode","@end","@implementation","@interface","@private","@protected","@protocol","@public","@selector","self","super"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["auto","char","const","double","float","int","long","register","short","signed","static","unsigned","void","volatile"] >>= withAttribute "Data Type"))
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
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "MultiLineComment")
                        <|>
                        ((pAnyChar ":!%&()+,-/.*<=>?[]|~^;" >>= withAttribute "Symbol"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "#") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pDetect2Chars False '@' '"' >>= withAttribute "String") >>~ pushContext "String"))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "String"))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "SingleLineComment" = 
  pzero

parseRules "MultiLineComment" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
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
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "MultiLineCommentPrep"))
     return (attr, result)

parseRules "MultiLineCommentPrep" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
