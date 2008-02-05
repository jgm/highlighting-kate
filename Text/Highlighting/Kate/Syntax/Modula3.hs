{- This module was generated from data in the Kate syntax highlighting file modula-3.xml, version 1.01,
   by   -}

module Text.Highlighting.Kate.Syntax.Modula3 ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Modula-3"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.m3;*.i3;*.ig;*.mg;"

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
  setState $ st { synStLanguage = "Modula-3" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Modula-3",["Normal"])], synStLanguage = "Modula-3", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String1" -> (popContext >> return ())
    "Comment2" -> return ()
    "Prep1" -> return ()
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Operator","Keyword"),("Type","DataType"),("Integer","BaseN"),("Real","Float"),("Constant","DecVal"),("String","String"),("Char","Char"),("Pervasive","Function"),("StdLib","Function"),("Comment","Comment"),("Pragma","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String1","String"),("Comment2","Comment"),("Prep1","Pragma")]

parseRules "Normal" = 
  do (attr, result) <- (((pRegExpr (compileRegex "PROCEDURE[\\s].*\\(") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "END\\s*[A-Za-z][A-Za-z0-9_]*\\;") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\b(RECORD|OBJECT|TRY|WHILE|FOR|REPEAT|LOOP|IF|CASE|WITH)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\b(END;|END)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["ANY","ARRAY","AS","BEGIN","BITS","BRANDED","BY","CASE","CONST","DO","ELSE","ELSIF","END","EVAL","EXCEPT","EXCEPTION","EXIT","EXPORTS","FINALLY","FOR","FROM","GENERIC","IF","IMPORT","INTERFACE","LOCK","LOOP","METHODS","MODULE","OBJECT","OF","OVERRIDES","PROCEDURE","RAISE","RAISES","READONLY","RECORD","REF","REPEAT","RETURN","REVEAL","ROOT","SET","THEN","TO","TRY","TYPE","TYPECASE","UNSAFE","UNTIL","UNTRACED","VALUE","VAR","WHILE","WITH"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["AND","DIV","IN","MOD","NOT","OR","+","<","#","=",";","..",":","-",">","{","}","|",":=","<:","*","<=","(",")","^",",","=>","/",">=","[","]",".","&"] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["ADDRESS","BOOLEAN","CARDINAL","CHAR","EXTENDED","INTEGER","LONGREAL","MUTEX","NULL","REAL","REFANY","T","TEXT"] >>= withAttribute "Type"))
                        <|>
                        ((pKeyword ["FALSE","NIL","TRUE"] >>= withAttribute "Constant"))
                        <|>
                        ((pKeyword ["ABS","ADR","ADRSIZE","BITSIZE","BYTESIZE","CEILING","DEC","DISPOSE","FIRST","FLOAT","FLOOR","INC","ISTYPE","LAST","LOOPHOLE","MAX","MIN","NARROW","NEW","NUMBER","ORD","ROUND","SUBARRAY","TRUNC","TYPECODE","VAL"] >>= withAttribute "Pervasive"))
                        <|>
                        ((pKeyword ["Text","Text.Length","Text.Empty","Text.Equal","Text.Compare","Text.Cat","Text.Sub","Text.Hash","Text.HasWideChar","Text.GetChar","Text.GetWideChar","Text.SetChars","Text.SetWideChars","Text.FromChars","Text.FromWideChars","Text.FindChar","Text.FindWideChar","Text.FindCharR","Text.FindWideCharR","Fmt","Fmt.Bool","Fmt.Char","Fmt.Int","Fmt.Unsigned","Fmt.Real","Fmt.LongReal","Fmt.Extended","Fmt.Pad","Fmt.F","Fmt.FN","Scan","Scan.Bool","Scan.Int","Scan.Unsigned","Scan.Real","Scan.LongReal","Scan.Extended","IO","IO.Put","IO.PutChar","IO.PutWideChar","IO.PutInt","IO.PutReal","IO.EOF","IO.GetLine","IO.GetChar","IO.GetWideChar","IO.GetInt","IO.GetReal","IO.OpenRead","IO.OpenWrite","Rd","Rd.GetChar","Rd.GetWideChar","Rd.EOF","Rd.UnGetChar","Rd.CharsReady","Rd.GetSub","Rd.GetWideSub","Rd.GetSubLine","Rd.GetWideSubLine","Rd.GetText","Rd.GetWideText","Rd.GetLine","Rd.GetWideLine","Rd.Seek","Rd.Close","Rd.Index","Rd.Length","Rd.Intermittend","Rd.Seekable","Rd.Closed","Wr","Wr.PutChar","Wr.PutWideChar","Wr.PutText","Wr.PutWideText","Wr.PutString","Wr.PutWideString","Wr.Seek","Wr.Flush","Wr.Close","Wr.Length","Wr.Index","Wr.Seekable","Wr.Closed","Wr.Buffered","Lex","Lex.Scan","Lex.Skip","Lex.Match","Lex.Bool","Lex.Int","Lex.Unsigned","Lex.Real","Lex.LongReal","Lex.Extended","Params","Params.Count","Params.Get","Env","Env.Count","Env.Get","Env.GetNth"] >>= withAttribute "StdLib"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[\\+|\\-]{0,1}[0-9]{1,}\\.[0-9]{1,}([E|e|D|d|X|x][\\+|\\-]{0,1}[0-9]{1,}){0,1}\\b") >>= withAttribute "Real"))
                        <|>
                        ((pRegExpr (compileRegex "\\b([\\+|\\-]{0,1}[0-9]{1,}|([2-9]|1[0-6])\\_[0-9A-Fa-f]{1,})\\b") >>= withAttribute "Integer"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String1")
                        <|>
                        ((pRegExpr (compileRegex "\\'(.|\\\\[ntrf\\\\'\"]|\\\\[0-7]{3})\\'") >>= withAttribute "Char"))
                        <|>
                        ((pDetect2Chars False '<' '*' >>= withAttribute "Pragma") >>~ pushContext "Prep1")
                        <|>
                        ((pDetect2Chars False '(' '*' >>= withAttribute "Comment") >>~ pushContext "Comment2"))
     return (attr, result)

parseRules "String1" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Comment2" = 
  do (attr, result) <- (((pDetect2Chars False '(' '*' >>= withAttribute "Comment") >>~ pushContext "Comment2")
                        <|>
                        ((pDetect2Chars False '*' ')' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Prep1" = 
  do (attr, result) <- ((pDetect2Chars False '*' '>' >>= withAttribute "Pragma") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x