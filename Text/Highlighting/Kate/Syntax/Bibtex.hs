{- This module was generated from data in the Kate syntax highlighting file bibtex.xml, version 1.13,
   by  Jeroen Wijnhout (Jeroen.Wijnhout@kdemail.net) -}

module Text.Highlighting.Kate.Syntax.Bibtex ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "BibTeX"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.bib"

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
  setState $ st { synStLanguage = "BibTeX" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("BibTeX",["Normal"])], synStLanguage = "BibTeX", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Entry" -> return ()
    "String" -> return ()
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

styles = [("Normal Text","Normal"),("Entry","Keyword"),("Command","Function"),("Field","DataType"),("Ref Key","Others"),("String","String"),("Char","Char")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("Entry","Ref Key"),("String","String")]

parseRules "Normal" = 
  do (attr, result) <- (((pFirstNonSpace >> pRegExpr (compileRegex "([a-zA-Z]+)\\s*=") >>= withAttribute "Field"))
                        <|>
                        ((pKeyword ["@article","@book","@booklet","@conference","@inbook","@incollection","@inproceedings","@manual","@mastersthesis","@misc","@phdthesis","@proceedings","@techreport","@unpublished","@collection","@patent"] >>= withAttribute "Entry") >>~ pushContext "Entry")
                        <|>
                        ((pKeyword ["@string","@preamble","@comment"] >>= withAttribute "Command"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\([a-zA-Z]+|.)") >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String"))
     return (attr, result)

parseRules "Entry" = 
  do (attr, result) <- (((pDetectChar False ',' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\([a-zA-Z]+|.)") >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\([a-zA-Z]+|.)") >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x