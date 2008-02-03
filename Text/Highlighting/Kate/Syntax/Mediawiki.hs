{- This module was generated from data in the Kate syntax highlighting file mediawiki.xml, version 1.02,
   by   -}

module Text.Highlighting.Kate.Syntax.Mediawiki ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "MediaWiki"

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
  setState $ st { synStLanguage = "MediaWiki" }
  context <- currentContext <|> (pushContext "normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("MediaWiki",["normal"])], synStLanguage = "MediaWiki", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "normal" -> return ()
    "Table" -> return ()
    "comment" -> return ()
    "URL" -> return ()
    "WikiLink" -> return ()
    "WikiLinkDescription" -> return ()
    "Link" -> return ()
    "Error" -> (popContext >> return ())
    "Template" -> return ()
    "NoWiki" -> return ()
    "Unformatted" -> (popContext >> return ())
    "Pre" -> return ()
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

styles = [("Normal","Normal"),("Link","Others"),("URL","Others"),("Comment","Comment"),("Section","Keyword"),("HTML-Entity","DecVal"),("HTML-Tag","Keyword"),("Wiki-Tag","DecVal"),("Error","Error"),("NoWiki","Normal"),("Unformatted","Normal")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("normal","Normal"),("Table","Normal"),("comment","Comment"),("URL","Link"),("WikiLink","Link"),("WikiLinkDescription","Link"),("Link","Template"),("Error","Error"),("Template","Link"),("NoWiki","NoWiki"),("Unformatted","Unformatted"),("Pre","NoWiki")]

parseRules "normal" = 
  do (attr, result) <- (((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "comment")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "([=]{2,2}[^=]+[=]{2,2}|[=]{3,3}[^=]+[=]{3,3}|[=]{4,4}[^=]+[=]{4,4}|[=]{5,5}[^=]+[=]{5,5})") >>= withAttribute "Section"))
                        <|>
                        ((pRegExpr (compileRegex "[~]{3,4}") >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "[*#;:\\s]*[*#:]+") >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pRegExpr (compileRegex "[[](?![[])") >>= withAttribute "Wiki-Tag") >>~ pushContext "URL")
                        <|>
                        ((pRegExpr (compileRegex "(http:|ftp:|mailto:)[\\S]*($|[\\s])") >>= withAttribute "URL"))
                        <|>
                        ((pRegExpr (compileRegex "[']{2,}") >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pColumn 0 >> pDetect2Chars False '{' '|' >>= withAttribute "Wiki-Tag") >>~ pushContext "Table")
                        <|>
                        ((pDetect2Chars False '{' '{' >>= withAttribute "Wiki-Tag") >>~ pushContext "Template")
                        <|>
                        ((pDetect2Chars False '[' '[' >>= withAttribute "Wiki-Tag") >>~ pushContext "WikiLink")
                        <|>
                        ((pRangeDetect '&' ';' >>= withAttribute "HTML-Entity"))
                        <|>
                        ((pString False "<nowiki>" >>= withAttribute "Wiki-Tag") >>~ pushContext "NoWiki")
                        <|>
                        ((pString False "<pre>" >>= withAttribute "HTML-Tag") >>~ pushContext "Pre")
                        <|>
                        ((pRegExpr (compileRegex "[<][^>]+[>]") >>= withAttribute "HTML-Tag"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "[\\s]") >>= withAttribute "Normal") >>~ pushContext "Unformatted"))
     return (attr, result)

parseRules "Table" = 
  do (attr, result) <- (((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "comment")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "([=]{2,2}[^=]+[=]{2,2}|[=]{3,3}[^=]+[=]{3,3}|[=]{4,4}[^=]+[=]{4,4}|[=]{5,5}[^=]+[=]{5,5})") >>= withAttribute "Section"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "[*#;:\\s]*[*#:]+") >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pRegExpr (compileRegex "[[](?![[])") >>= withAttribute "Wiki-Tag") >>~ pushContext "URL")
                        <|>
                        ((pRegExpr (compileRegex "(http:|ftp:|mailto:)[\\S]*($|[\\s])") >>= withAttribute "URL"))
                        <|>
                        ((pRegExpr (compileRegex "[']{2,}") >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pColumn 0 >> pDetect2Chars False '|' '}' >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '|' >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pDetect2Chars False '{' '{' >>= withAttribute "Wiki-Tag") >>~ pushContext "Template")
                        <|>
                        ((pDetect2Chars False '[' '[' >>= withAttribute "Wiki-Tag") >>~ pushContext "WikiLink")
                        <|>
                        ((pRangeDetect '&' ';' >>= withAttribute "HTML-Entity"))
                        <|>
                        ((pString False "<nowiki>" >>= withAttribute "Wiki-Tag") >>~ pushContext "NoWiki")
                        <|>
                        ((pString False "<pre>" >>= withAttribute "HTML-Tag") >>~ pushContext "Pre")
                        <|>
                        ((pRegExpr (compileRegex "[<][^>]+[>]") >>= withAttribute "HTML-Tag"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "[\\s]") >>= withAttribute "Normal") >>~ pushContext "Unformatted")
                        <|>
                        ((pRegExpr (compileRegex "[~]{3,4}") >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pRegExpr (compileRegex "[-]{4,}") >>= withAttribute "Wiki-Tag"))
                        <|>
                        ((pColumn 0 >> pDetectChar False '!' >>= withAttribute "Wiki-Tag")))
     return (attr, result)

parseRules "comment" = 
  do (attr, result) <- ((pString False "-->" >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "URL" = 
  do (attr, result) <- (((pDetectChar False ']' >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Error") >>~ pushContext "Error"))
     return (attr, result)

parseRules "WikiLink" = 
  do (attr, result) <- (((pDetectChar False '|' >>= withAttribute "Wiki-Tag") >>~ pushContext "WikiLinkDescription")
                        <|>
                        ((pDetect2Chars False ']' ']' >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Error") >>~ pushContext "Error"))
     return (attr, result)

parseRules "WikiLinkDescription" = 
  do (attr, result) <- ((pDetect2Chars False ']' ']' >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Link" = 
  do (attr, result) <- (((pDetect2Chars False '}' '}' >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pAnyChar "'[]" >>= withAttribute "Error") >>~ pushContext "Error"))
     return (attr, result)

parseRules "Error" = 
  pzero

parseRules "Template" = 
  do (attr, result) <- (((pDetect2Chars False '}' '}' >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Error") >>~ pushContext "Error"))
     return (attr, result)

parseRules "NoWiki" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<!--[^-]*-->") >>= withAttribute "NoWiki"))
                        <|>
                        ((pString False "</nowiki>" >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[<][^>]+[>]") >>= withAttribute "HTML-Tag"))
                        <|>
                        ((pString False "<pre>" >>= withAttribute "HTML-Tag") >>~ pushContext "Pre"))
     return (attr, result)

parseRules "Unformatted" = 
  pzero

parseRules "Pre" = 
  do (attr, result) <- ((pString False "</pre>" >>= withAttribute "Wiki-Tag") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x