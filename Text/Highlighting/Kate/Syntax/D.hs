{- This module was generated from data in the Kate syntax highlighting file d.xml, version 1.43,
   by  Aziz Köksal (aziz.koeksal@gmail.com), Jari-Matti Mäkelä (jmjm@iki.fi), Simon J Mackenzie (project.katedxml@smackoz.fastmail.fm) -}

module Text.Highlighting.Kate.Syntax.D ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "D"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.d;*.D;*.di;*.DI;"

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
  setState $ st { synStLanguage = "D" }
  context <- currentContext <|> (pushContext "normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("D",["normal"])], synStLanguage = "D", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "normal" -> return ()
    "UnicodeShort" -> (popContext >> return ())
    "UnicodeLong" -> (popContext >> return ())
    "HTMLEntity" -> (popContext >> return ())
    "ModuleName" -> return ()
    "Deprecated" -> return ()
    "Linkage" -> (popContext >> return ())
    "Linkage2" -> (popContext >> return ())
    "Version" -> (popContext >> return ())
    "Version2" -> (popContext >> return ())
    "Pragmas" -> (popContext >> return ())
    "RawString" -> return ()
    "BQString" -> return ()
    "HexString" -> return ()
    "CharLiteral" -> (popContext >> return ())
    "String" -> return ()
    "CommentLine" -> (popContext >> return ())
    "CommentBlock" -> return ()
    "CommentNested" -> return ()
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Type","DataType"),("Integer","DecVal"),("Binary","BaseN"),("Octal","BaseN"),("Hex","BaseN"),("Float","Float"),("LibrarySymbols","DataType"),("Deprecated","Comment"),("SpecialTokens","Normal"),("Module","Keyword"),("Module Name","Normal"),("Linkage","Keyword"),("Linkage Type","Normal"),("Debug","Keyword"),("Assert","Keyword"),("Version","Keyword"),("Version Type","Normal"),("Unit Test","Keyword"),("Pragma","Keyword"),("EscapeString","String"),("EscapeSequence","String"),("String","String"),("Char","Char"),("RawString","String"),("BQString","String"),("HexString","String"),("Comment","Comment"),("Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("normal","Normal Text"),("UnicodeShort","EscapeString"),("UnicodeLong","EscapeString"),("HTMLEntity","EscapeString"),("ModuleName","Module Name"),("Deprecated","Deprecated"),("Linkage","Linkage"),("Linkage2","Linkage"),("Version","Version"),("Version2","Version"),("Pragmas","Pragma"),("RawString","RawString"),("BQString","BQString"),("HexString","HexString"),("CharLiteral","Char"),("String","String"),("CommentLine","Comment"),("CommentBlock","Comment"),("CommentNested","Comment")]

parseRules "normal" = 
  do (attr, result) <- (((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["abstract","alias","align","asm","auto","body","break","case","cast","catch","class","const","continue","default","delegate","delete","do","else","enum","export","false","final","finally","for","foreach","foreach_reverse","function","goto","if","in","inout","interface","invariant","is","lazy","macro","mixin","new","null","out","override","package","private","protected","public","ref","return","scope","static","struct","super","switch","synchronized","template","this","throw","true","try","typedef","typeid","typeof","union","volatile","while","with"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["module","import"] >>= withAttribute "Module") >>~ pushContext "ModuleName")
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["void","bool","byte","ubyte","short","ushort","int","uint","long","ulong","cent","ucent","float","double","real","ireal","ifloat","idouble","creal","cfloat","cdouble","char","wchar","dchar"] >>= withAttribute "Type"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["string","wstring","dstring","size_t","ptrdiff_t","hash_t","Error","Exception","Object","TypeInfo","ClassInfo"] >>= withAttribute "LibrarySymbols"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["extern"] >>= withAttribute "Linkage") >>~ pushContext "Linkage")
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["debug"] >>= withAttribute "Debug"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["assert"] >>= withAttribute "Assert"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["pragma"] >>= withAttribute "Pragma") >>~ pushContext "Pragmas")
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["version"] >>= withAttribute "Version") >>~ pushContext "Version")
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["unittest"] >>= withAttribute "Unit Test"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["__FILE__","__LINE__","__DATE__","__TIME__","__TIMESTAMP__","__VENDOR__","__VERSION__","__EOF__"] >>= withAttribute "SpecialTokens"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["deprecated"] >>= withAttribute "Deprecated") >>~ pushContext "Deprecated")
                        <|>
                        ((pDetect2Chars False 'r' '"' >>= withAttribute "RawString") >>~ pushContext "RawString")
                        <|>
                        ((pDetect2Chars False 'x' '"' >>= withAttribute "HexString") >>~ pushContext "HexString")
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pHlCStringChar >>= withAttribute "EscapeString") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '\\' 'u' >>= withAttribute "EscapeString") >>~ pushContext "UnicodeShort")
                        <|>
                        ((pDetect2Chars False '\\' 'U' >>= withAttribute "EscapeString") >>~ pushContext "UnicodeLong")
                        <|>
                        ((pDetect2Chars False '\\' '&' >>= withAttribute "EscapeString") >>~ pushContext "HTMLEntity")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Char") >>~ pushContext "CharLiteral")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "BQString") >>~ pushContext "BQString")
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "CommentLine")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "CommentBlock")
                        <|>
                        ((pDetect2Chars False '/' '+' >>= withAttribute "Comment") >>~ pushContext "CommentNested")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text"))
                        <|>
                        ((pString False "..." >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '.' '.' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "0[xX][_a-fA-F\\d]*(\\.[_a-fA-F\\d]*)?[pP][-+]?[\\d]+[_\\d]*[fFL]?i?") >>= withAttribute "Float") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[\\d][_\\d]*(\\.(?!\\.)[_\\d]*([eE][-+]?[\\d]+[_\\d]*)?[fFL]?i?|[eE][-+]?[\\d]+[_\\d]*[fFL]?i?|[fF]i?|[fFL]?i)") >>= withAttribute "Float") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\.[\\d][_\\d]*([eE][-+]?[\\d]+[_\\d]*)?[fFL]?i?") >>= withAttribute "Float") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "0[bB]_*[01][01_]*(L[uU]?|[uU]L?)?") >>= withAttribute "Binary") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "0_*[0-7][0-7_]*(L[uU]?|[uU]L?)?") >>= withAttribute "Octal") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "0[xX]_*[\\da-fA-F][\\da-fA-F_]*(L[uU]?|[uU]L?)?") >>= withAttribute "Hex") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\d+[\\d_]*(L[uU]?|[uU]L?)?") >>= withAttribute "Integer") >>~ (popContext >> return ()))
                        <|>
                        ((pString False "#line" >>= withAttribute "Pragma") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "UnicodeShort" = 
  do (attr, result) <- ((pRegExpr (compileRegex "[\\da-fA-F]{4}") >>= withAttribute "EscapeString") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "UnicodeLong" = 
  do (attr, result) <- ((pRegExpr (compileRegex "[\\da-fA-F]{8}") >>= withAttribute "EscapeString") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "HTMLEntity" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[a-zA-Z]\\w+;") >>= withAttribute "EscapeString") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "ModuleName" = 
  do (attr, result) <- (((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "CommentLine")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "CommentBlock")
                        <|>
                        ((pDetect2Chars False '/' '+' >>= withAttribute "Comment") >>~ pushContext "CommentNested")
                        <|>
                        ((pRegExpr (compileRegex "[^\\s\\w.:,]") >>= withAttribute "Module Name") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Deprecated" = 
  do (attr, result) <- (((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "CommentLine")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "CommentBlock")
                        <|>
                        ((pDetect2Chars False '/' '+' >>= withAttribute "Comment") >>~ pushContext "CommentNested")
                        <|>
                        ((pRegExpr (compileRegex "[;({=]") >>= withAttribute "Normal Text") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Linkage" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Linkage"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "Linkage2")
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Linkage2" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Linkage"))
                        <|>
                        ((pString False "C++" >>= withAttribute "Linkage Type") >>~ (popContext >> return ()))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["C","D","Windows","Pascal","System"] >>= withAttribute "Linkage Type") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^)]+") >>= withAttribute "Error") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Version" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Version"))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text") >>~ pushContext "Version2")
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "Version2")
                        <|>
                        ((pDetectChar False ';' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^\\n]+") >>= withAttribute "Error") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Version2" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Version"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["DigitalMars","X86","AMD64","Windows","Win32","Win64","linux","LittleEndian","BigEndian","D_InlineAsm","none"] >>= withAttribute "Version Type") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pInt >>= withAttribute "Integer") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^);]+") >>= withAttribute "Error") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Pragmas" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Normal Text"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["msg","lib"] >>= withAttribute "Version Type") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "RawString" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "RawString") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "BQString" = 
  do (attr, result) <- ((pDetectChar False '`' >>= withAttribute "BQString") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "HexString" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "HexString") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^\\sa-fA-F\\d\"]+") >>= withAttribute "Error")))
     return (attr, result)

parseRules "CharLiteral" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "EscapeSequence"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Char") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\\\(u[\\da-fA-F]{4}|U[\\da-fA-F]{8}|&[a-zA-Z]\\w+;)") >>= withAttribute "EscapeSequence"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Char") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex ".'") >>= withAttribute "Char"))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pDetect2Chars False '\\' '"' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pHlCStringChar >>= withAttribute "EscapeSequence"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\(u[\\da-fA-F]{4}|U[\\da-fA-F]{8}|&[a-zA-Z]\\w+;)") >>= withAttribute "EscapeSequence")))
     return (attr, result)

parseRules "CommentLine" = 
  pzero

parseRules "CommentBlock" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "CommentNested" = 
  do (attr, result) <- (((pDetect2Chars False '/' '+' >>= withAttribute "Comment") >>~ pushContext "CommentNested")
                        <|>
                        ((pDetect2Chars False '+' '/' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
