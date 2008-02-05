{- This module was generated from data in the Kate syntax highlighting file prolog.xml, version 1.04,
   by   -}

module Text.Highlighting.Kate.Syntax.Prolog ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Prolog"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.prolog"

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
  setState $ st { synStLanguage = "Prolog" }
  context <- currentContext <|> (pushContext "normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Prolog",["normal"])], synStLanguage = "Prolog", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "normal" -> return ()
    "comment" -> (popContext >> return ())
    "string" -> return ()
    "string2" -> return ()
    "comment region" -> return ()
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Data Type","DataType"),("Comment","Comment"),("Integer","DecVal"),("Symbol","Normal"),("String","String"),("Identifier","Normal"),("Variable","Others"),("Arithmetic","Keyword")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("normal","Symbol"),("comment","Comment"),("string","String"),("string2","String"),("comment region","Comment")]

parseRules "normal" = 
  do (attr, result) <- (((pKeyword ["abstract","align","as","and","class","clauses","constants","database","determ","domains","elsedef","endclass","enddef","erroneous","facts","failure","global","goal","if","ifdef","ifndef","implement","include","language","multi","nocopy","nondeterm","object","or","procedure","protected","predicates","reference","single","static","struct","this"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["ABSTRACT","ALIGN","AS","AND","CLASS","CLAUSES","CONSTANTS","DATABASE","DETERM","DOMAINS","ELSEDEF","ENDCLASS","ENDDEF","ERRONEOUS","FACTS","FAILURE","GLOBAL","GOAL","IF","IFDEF","IFNDEF","IMPLEMENT","INCLUDE","LANGUAGE","MULTI","NOCOPY","NONDETERM","OBJECT","OR","PROCEDURE","PROTECTED","PREDICATES","REFERENCE","SINGLE","STATIC","STRUCT","THIS"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["mod","div","abs","exp","ln","log","sqrt","round","trunc","val","cos","sin","tan","arctan","random","randominit"] >>= withAttribute "Arithmetic"))
                        <|>
                        ((pKeyword ["bgidriver","bgifont","check_determ","code","config","diagnostics","error","errorlevel","heap","gstacksize","nobreak","nowarnings","printermenu","project"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["assert","asserta","assertz","bound","chain_inserta","chain_insertafter","chain_insertz","chain_terms","consult","db_btrees","db_chains","fail","findall","format","free","msgrecv","msgsend","nl","not","readterm","ref_term","retract","retractall","save","term_bin","term_replace","term_str","trap","write","writef"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["char","real","string","symbol","byte","sbyte","short","ushort","word","integer","unsigned","dword","long","ulong","binary","ref"] >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "[A-Z_][A-Za-z0-9_]*") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[a-z][A-Za-z0-9_]*") >>= withAttribute "Identifier"))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "comment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "comment region")
                        <|>
                        ((pInt >>= withAttribute "Integer"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "string")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "string2")
                        <|>
                        ((pAnyChar "~!^*()-+=[]|\\:;,./?&<>" >>= withAttribute "Symbol")))
     return (attr, result)

parseRules "comment" = 
  pzero

parseRules "string" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "string2" = 
  do (attr, result) <- ((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "comment region" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x