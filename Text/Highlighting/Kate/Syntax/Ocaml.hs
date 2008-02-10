{- This module was generated from data in the Kate syntax highlighting file ocaml.xml, version 1.04,
   by  Glyn Webster (glyn@wave.co.nz) -}

module Text.Highlighting.Kate.Syntax.Ocaml ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Objective Caml"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.ml;*.mli"

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
  setState $ st { synStLanguage = "Objective Caml" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Objective Caml",["Normal"])], synStLanguage = "Objective Caml", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Multiline Comment" -> return ()
    "String Constant" -> return ()
    "Camlp4 Quotation Constant" -> return ()
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

styles = [("Normal Text","Normal"),("Identifier","Normal"),("Keyword","Keyword"),("Revised Syntax Keyword","Normal"),("Core Data Type","DataType"),("Decimal","DecVal"),("Hexadecimal","BaseN"),("Octal","BaseN"),("Binary","BaseN"),("Float","Float"),("Character","Char"),("String","String"),("Escaped characters","Char"),("Comment","Comment"),("Camlp4 Quotation","String"),("Directive","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("Multiline Comment","Comment"),("String Constant","String"),("Camlp4 Quotation Constant","Camlp4 Quotation")]

parseRules "Normal" = 
  do (attr, result) <- (((pDetect2Chars False '(' '*' >>= withAttribute "Comment") >>~ pushContext "Multiline Comment")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#[A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\0377_][A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\03770-9_']*.*$") >>= withAttribute "Directive"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String Constant")
                        <|>
                        ((pRegExpr (compileRegex "'((\\\\[ntbr'\"\\\\]|\\\\[0-9]{3}|\\\\x[0-9A-Fa-f]{2})|[^'])'") >>= withAttribute "Character"))
                        <|>
                        ((pDetect2Chars False '<' '<' >>= withAttribute "Camlp4 Quotation") >>~ pushContext "Camlp4 Quotation Constant")
                        <|>
                        ((pRegExpr (compileRegex "<:[A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\0377_][A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\03770-9_']*<") >>= withAttribute "Camlp4 Quotation") >>~ pushContext "Camlp4 Quotation Constant")
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["and","as","assert","asr","begin","class","closed","constraint","do","done","downto","else","end","exception","external","false","for","fun","function","functor","if","in","include","inherit","land","lazy","let","lor","lsl","lsr","lxor","match","method","mod","module","mutable","new","of","open","or","parser","private","rec","sig","struct","then","to","true","try","type","val","virtual","when","while","with"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["declare","value","where"] >>= withAttribute "Revised Syntax Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["exn","lazy_t","format","unit","int","real","char","string","ref","array","bool","list","option"] >>= withAttribute "Core Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "[A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\0377_][A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\03770-9_']*") >>= withAttribute "Identifier"))
                        <|>
                        ((pRegExpr (compileRegex "-?0[xX][0-9A-Fa-f_]+") >>= withAttribute "Hexadecimal"))
                        <|>
                        ((pRegExpr (compileRegex "-?0[oO][0-7_]+") >>= withAttribute "Octal"))
                        <|>
                        ((pRegExpr (compileRegex "-?0[bB][01_]+") >>= withAttribute "Binary"))
                        <|>
                        ((pRegExpr (compileRegex "-?[0-9][0-9_]*(\\.[0-9][0-9_]*([eE][-+]?[0-9][0-9_]*)?|[eE][-+]?[0-9][0-9_]*)") >>= withAttribute "Float"))
                        <|>
                        ((pRegExpr (compileRegex "-?[0-9][0-9_]*") >>= withAttribute "Decimal")))
     return (attr, result)

parseRules "Multiline Comment" = 
  do (attr, result) <- (((pDetect2Chars False '*' ')' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '(' '*' >>= withAttribute "Comment") >>~ pushContext "Multiline Comment"))
     return (attr, result)

parseRules "String Constant" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "(\\\\[ntbr'\"\\\\]|\\\\[0-9]{3}|\\\\x[0-9A-Fa-f]{2})") >>= withAttribute "Escaped characters"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\$") >>= withAttribute "Escaped characters")))
     return (attr, result)

parseRules "Camlp4 Quotation Constant" = 
  do (attr, result) <- (((pDetect2Chars False '>' '>' >>= withAttribute "Camlp4 Quotation") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '<' '<' >>= withAttribute "Camlp4 Quotation") >>~ pushContext "Camlp4 Quotation Constant")
                        <|>
                        ((pRegExpr (compileRegex "<:[A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\0377_][A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\03770-9_']*<") >>= withAttribute "Camlp4 Quotation") >>~ pushContext "Camlp4 Quotation Constant")
                        <|>
                        ((pRegExpr (compileRegex "\\\\(\\\\|>>|<<)") >>= withAttribute "Escaped characters"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\<:[A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\0377_][A-Za-z\\0300-\\0326\\0330-\\0366\\0370-\\03770-9_']*<") >>= withAttribute "Escaped characters")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
