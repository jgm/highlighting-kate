{- This module was generated from data in the Kate syntax highlighting file awk.xml, version 0.90,
   by   -}

module Text.Highlighting.Kate.Syntax.Awk ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "AWK"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.awk"

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
  setState $ st { synStLanguage = "AWK" }
  context <- currentContext <|> (pushContext "Base" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("AWK",["Base"])], synStLanguage = "AWK", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Base" -> return ()
    "String" -> return ()
    "Comment" -> (popContext >> return ())
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

styles = [("Normal","Normal"),("Keyword","Keyword"),("Builtin","DataType"),("Function","Function"),("Decimal","DecVal"),("Float","Float"),("String","String"),("Comment","Comment"),("Pattern","String"),("Field","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Base","Normal"),("String","String"),("Comment","Comment")]

parseRules "Base" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\b(BEGIN|END)\\b") >>= withAttribute "Pattern"))
                        <|>
                        ((pRegExpr (compileRegex "/([^\\/[]|\\\\.|\\[\\]?(\\[[^]]+\\]|.)+\\])+/") >>= withAttribute "Pattern"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Keyword"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Keyword"))
                        <|>
                        ((pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pKeyword ["if","else","while","do","for","in","continue","break","print","printf","getline","function","return","next","exit"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["ARGC","ARGV","CONVFMT","ENVIRON","FILENAME","FNR","FS","NF","NR","OFMT","OFS","ORS","RS","RSTART","RLENGTH","SUBSEP"] >>= withAttribute "Builtin"))
                        <|>
                        ((pKeyword ["gsub","gensub","index","length","match","split","sprintf","sub","substr","tolower","toupper","atan2","cos","exp","int","log","rand","sin","sqrt","srand","close","fflush","system"] >>= withAttribute "Function"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pRegExpr (compileRegex "\\$[A-Za-z0-9_]+") >>= withAttribute "Field")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String")))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x