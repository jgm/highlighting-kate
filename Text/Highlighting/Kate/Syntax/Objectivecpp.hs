{- This module was generated from data in the Kate syntax highlighting file objectivecpp.xml, version 1.00,
   by  Gennady Telegin (gepo@lvk.cs.msu.su -}

module Text.Highlighting.Kate.Syntax.Objectivecpp ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
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
syntaxName = "Objective-C++"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.mm;*.M;*.h"

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
  setState $ st { synStLanguage = "Objective-C++" }
  context <- currentContext <|> (pushContext "Default" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Objective-C++",["Default"])], synStLanguage = "Objective-C++", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

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
    "Region Marker" -> (popContext >> return ())
    "Commentar 1" -> (popContext >> return ())
    "Commentar 2" -> return ()
    "Preprocessor" -> (popContext >> return ())
    "Define" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Extensions","Keyword"),("Data Type","DataType"),("Decimal","DecVal"),("Octal","BaseN"),("Hex","BaseN"),("Float","Float"),("Char","Char"),("String","String"),("String Char","Char"),("Comment","Comment"),("Symbol","Normal"),("Preprocessor","Others"),("Prep. Lib","Others"),("Region Marker","RegionMarker")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Default","Normal Text"),("String","String"),("SingleLineComment","Comment"),("MultiLineComment","Comment"),("Preprocessor","Preprocessor"),("MultiLineCommentPrep","Comment"),("Region Marker","Region Marker"),("Commentar 1","Comment"),("Commentar 2","Comment"),("Preprocessor","Preprocessor"),("Define","Preprocessor"),("Outscoped","Comment"),("Outscoped intern","Comment")]

parseRules "Default" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*if\\s+0") >>= withAttribute "Preprocessor") >>~ pushContext "Outscoped")
                        <|>
                        ((pFirstNonSpace >> pDetectChar False '#' >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pFirstNonSpace >> pString False "//BEGIN" >>= withAttribute "Region Marker") >>~ pushContext "Region Marker")
                        <|>
                        ((pFirstNonSpace >> pString False "//END" >>= withAttribute "Region Marker") >>~ pushContext "Region Marker")
                        <|>
                        ((pKeyword ["break","case","continue","default","do","else","enum","extern","for","goto","if","return","sizeof","struct","switch","typedef","union","while","@class","@defs","@encode","@end","@implementation","@interface","@private","@protected","@protocol","@public","@selector","self","super","asm","catch","class","const_cast","delete","dynamic_cast","explicit","export","false","friend","inline","namespace","new","operator","private","protected","public","qobject_cast","reinterpret_cast","static_cast","template","this","throw","true","try","typeid","type_info","typename","using","virtual","and","and_eq","bad_cast","bad_typeid","bitand","bitor","compl","not","not_eq","or","or_eq","xor","xor_eq","except","finally","xalloc"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["auto","char","const","double","float","int","long","register","short","signed","static","unsigned","void","volatile","bool","mutable","uchar","uint","int8_t","int16_t","int32_t","int64_t","uint8_t","uint16_t","uint32_t","uint64_t","wchar_t"] >>= withAttribute "Data Type"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
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
                        ((pColumn 0 >> pRegExpr (compileRegex "#") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pDetect2Chars False '@' '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pKeyword ["K_DCOP","SLOT","SIGNAL","Q_CLASSINFO","Q_ENUMS","Q_EXPORT","Q_OBJECT","Q_OVERRIDE","Q_PROPERTY","Q_SETS","Q_SIGNALS","Q_SLOTS","Q_FOREACH","Q_DECLARE_FLAGS","Q_INIT_RESOURCE","Q_CLEANUP_RESOURCE","Q_GLOBAL_STATIC","Q_GLOBAL_STATIC_WITH_ARGS","Q_DECLARE_INTERFACE","Q_DECLARE_TYPEINFO","Q_DECLARE_SHARED","Q_DECLARE_FLAGS","Q_DECLARE_OPERATORS_FOR_FLAGS","Q_FOREVER","Q_DECLARE_PRIVATE","Q_DECLARE_PUBLIC","Q_D","Q_Q","Q_DISABLE_COPY","Q_INTERFACES","Q_FLAGS","Q_SCRIPTABLE","Q_INVOKABLE","Q_GADGET","Q_ARG","Q_RETURN_ARG","Q_ASSERT","Q_ASSERT_X","TRUE","FALSE","connect","disconnect","emit","signals","slots","foreach","forever"] >>= withAttribute "Extensions"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Doxygen.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Commentar 1")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Commentar 2")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Symbol"))
                        <|>
                        ((pAnyChar ":!%&()+,-/.*<=>?[]{|}~^;" >>= withAttribute "Symbol")))
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
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules "Region Marker" = 
  pzero

parseRules "Commentar 1" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules "Commentar 2" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules "Preprocessor" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Preprocessor"))
                        <|>
                        ((pRegExpr (compileRegex "define.*((?=\\\\))") >>= withAttribute "Preprocessor") >>~ pushContext "Define")
                        <|>
                        ((pRegExpr (compileRegex "define.*") >>= withAttribute "Preprocessor"))
                        <|>
                        ((pRangeDetect '"' '"' >>= withAttribute "Prep. Lib"))
                        <|>
                        ((pRangeDetect '<' '>' >>= withAttribute "Prep. Lib"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Doxygen.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Commentar 1")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "MultiLineCommentPrep"))
     return (attr, result)

parseRules "Define" = 
  do (attr, result) <- ((pLineContinue >>= withAttribute "Preprocessor"))
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
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*(endif|else|elif)") >>= withAttribute "Preprocessor") >>~ (popContext >> return ())))
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