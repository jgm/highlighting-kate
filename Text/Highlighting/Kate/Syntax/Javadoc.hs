{- This module was generated from data in the Kate syntax highlighting file javadoc.xml, version 1.03,
   by  Alfredo Luiz Foltran Fialho (alfoltran@ig.com.br) -}

module Text.Highlighting.Kate.Syntax.Javadoc ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Html
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Javadoc"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = ""

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
  setState $ st { synStLanguage = "Javadoc" }
  context <- currentContext <|> (pushContext "Start" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Javadoc",["Start"])], synStLanguage = "Javadoc", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Start" -> return ()
    "FindJavadoc" -> return ()
    "JavadocFSar" -> return ()
    "Javadocar" -> return ()
    "JavadocParam" -> (popContext >> return ())
    "InlineTagar" -> (popContext >> return ())
    "LiteralTagar" -> (popContext >> return ())
    "SeeTag" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("BlockTag","Keyword"),("InlineTag","Keyword"),("JavadocParam","Keyword"),("SeeTag","Keyword"),("JavadocFS","Comment"),("Javadoc","Comment")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Start","Normal Text"),("FindJavadoc","Normal Text"),("JavadocFSar","JavadocFS"),("Javadocar","Javadoc"),("JavadocParam","Javadoc"),("InlineTagar","InlineTag"),("LiteralTagar","InlineTag"),("SeeTag","SeeTag")]

parseRules "Start" = 
  do (attr, result) <- ((parseRules "FindJavadoc"))
     return (attr, result)

parseRules "FindJavadoc" = 
  do (attr, result) <- ((pString False "/**" >>= withAttribute "JavadocFS") >>~ pushContext "JavadocFSar")
     return (attr, result)

parseRules "JavadocFSar" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "JavadocFS") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "(!|\\?)") >>= withAttribute "JavadocFS") >>~ pushContext "Javadocar")
                        <|>
                        ((pRegExpr (compileRegex "(\\.\\s*$)") >>= withAttribute "JavadocFS") >>~ pushContext "Javadocar")
                        <|>
                        ((pRegExpr (compileRegex "(\\.\\s)(?![\\da-z])") >>= withAttribute "JavadocFS") >>~ pushContext "Javadocar")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "\\**\\s*(?=@(author|deprecated|exception|param|return|see|serial|serialData|serialField|since|throws|version)(\\s|$))") >>= withAttribute "JavadocFS") >>~ pushContext "Javadocar")
                        <|>
                        ((pString False "{@code " >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@code\t" >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@docRoot}" >>= withAttribute "InlineTag"))
                        <|>
                        ((pString False "{@inheritDoc}" >>= withAttribute "InlineTag"))
                        <|>
                        ((pString False "{@link " >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@link\t" >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@linkplain " >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@linkplain\t" >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@literal " >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@literal\t" >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@value}" >>= withAttribute "InlineTag"))
                        <|>
                        ((pString False "{@value " >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@value\t" >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Html.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Javadocar" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "JavadocFS") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "\\*+(?!/)") >>= withAttribute "JavadocFS"))
                        <|>
                        ((pString False "@author " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@deprecated " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@exception " >>= withAttribute "BlockTag") >>~ pushContext "JavadocParam")
                        <|>
                        ((pString False "@param " >>= withAttribute "BlockTag") >>~ pushContext "JavadocParam")
                        <|>
                        ((pString False "@return " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@see " >>= withAttribute "BlockTag") >>~ pushContext "SeeTag")
                        <|>
                        ((pString False "@serial " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@serialData " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@serialField " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@since " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@throws " >>= withAttribute "BlockTag") >>~ pushContext "JavadocParam")
                        <|>
                        ((pString False "@version " >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@author\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@deprecated\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@exception\t" >>= withAttribute "BlockTag") >>~ pushContext "JavadocParam")
                        <|>
                        ((pString False "@param\t" >>= withAttribute "BlockTag") >>~ pushContext "JavadocParam")
                        <|>
                        ((pString False "@return\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@see\t" >>= withAttribute "BlockTag") >>~ pushContext "SeeTag")
                        <|>
                        ((pString False "@serial\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@serialData\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@serialField\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@since\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "@throws\t" >>= withAttribute "BlockTag") >>~ pushContext "JavadocParam")
                        <|>
                        ((pString False "@version\t" >>= withAttribute "BlockTag"))
                        <|>
                        ((pString False "{@code " >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@code\t" >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@docRoot}" >>= withAttribute "InlineTag"))
                        <|>
                        ((pString False "{@inheritDoc}" >>= withAttribute "InlineTag"))
                        <|>
                        ((pString False "{@link " >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@link\t" >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@linkplain " >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@linkplain\t" >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@literal " >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@literal\t" >>= withAttribute "InlineTag") >>~ pushContext "LiteralTagar")
                        <|>
                        ((pString False "{@value}" >>= withAttribute "InlineTag"))
                        <|>
                        ((pString False "{@value " >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((pString False "{@value\t" >>= withAttribute "InlineTag") >>~ pushContext "InlineTagar")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Html.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "JavadocParam" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Javadoc"))
                        <|>
                        ((pRegExpr (compileRegex "\\S*(?=\\*/)") >>= withAttribute "JavadocParam") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S*(\\s|$)") >>= withAttribute "JavadocParam") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "InlineTagar" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "InlineTag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "JavadocFS") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Html.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "LiteralTagar" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "InlineTag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "JavadocFS") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "SeeTag" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "JavadocFS") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Html.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x