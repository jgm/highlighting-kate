{- This module was generated from data in the Kate syntax highlighting file lua.xml, version 1.02,
   by   -}

module Text.Highlighting.Kate.Syntax.Lua ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Doxygen
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Lua"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.lua"

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
  setState $ st { synStLanguage = "Lua" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Lua",["Normal"])], synStLanguage = "Lua", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Comment" -> (popContext >> return ())
    "Block Comment" -> return ()
    "String_single" -> pushContext "Error"
    "String_double" -> pushContext "Error"
    "String_block" -> return ()
    "Error" -> return ()
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

styles = [("Alerts","Alert"),("BFunc","Function"),("Comment","Comment"),("Constant","Keyword"),("Control","Keyword"),("Error","Error"),("Keyword","Keyword"),("Normal Text","Normal"),("Numbers","DecVal"),("Strings","String"),("Symbols","Others"),("Variable","Keyword")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("Comment","Comment"),("Block Comment","Comment"),("String_single","Strings"),("String_double","Strings"),("String_block","Strings"),("Error","Error")]

parseRules "Normal" = 
  do (attr, result) <- (((Text.Highlighting.Kate.Syntax.Doxygen.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pKeyword " \n\t():!+,-<=>%&*/;?[]^{|}~\\\"" ["table.foreach","table.foreachi","foreach","foreachi"] >>= withAttribute "Error"))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetect2Chars False '[' '[' >>= withAttribute "Strings") >>~ pushContext "String_block")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Strings") >>~ pushContext "String_single")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Strings") >>~ pushContext "String_double")
                        <|>
                        ((pKeyword " \n\t():!+,-<=>%&*/;?[]^{|}~\\\"" ["string.byte","string.char","string.find","string.len","string.lower","string.rep","string.sub","string.upper","string.format","string.gfind","string.gsub","table.concat","table.getn","table.sort","table.insert","table.remove","table.setn","math.abs","math.sin","math.cos","math.tan","math.asin","math.acos","math.atan","math.atan2","math.ceil","math.floor","math.mod","math.frexp","math.ldexp","math.squrt","math.min","math.max","math.log","math.log10","math.exp","math.deg","math.rad","math.random","math.randomseed","io.close","io.flush","io.input","io.lines","io.open","io.output","io.read","io.stderr","io.stdin","io.stdout","io.tmpfile","io.write","os.clock","os.date","os.difftime","os.execute","os.exit","os.getenv","os.remove","os.rename","os.setlocale","os.time","os.tmpname","debug.getinfo","debug.getlocal","debug.setlocal","debug.sethook","debug.gethook","assert","collectgarbage","dofile","error","next","print","rawget","rawset","tonumber","tostring","type","_ALERT","_ERRORMESSAGE","call","getmetatable","gcinfo","ipairs","loadfile","loadstring","pairs","pcall","require","LUA_PATH","setmetatable","_LOADED","_VERSION","gettagmethod","globals","newtag","setglobal","settag","settagmethod","setlinehook","getglobals","copytagmethods","dostring","getglobal","tag","setglobals","unpack","exit","readfrom","writeto","appendto","read","write","getinfo","getlocal","setlocal","setcallhook","tinsert","tremove","flush","seek","setlocale","execute","remove","rename","tmpname","getenv","getn","sort","table.foreach","table.foreachi","foreach","foreachi","abs","sin","cos","tan","asin","acos","atan","atan2","ceil","floor","mod","frexp","ldexp","squrt","min","max","log","log10","exp","deg","rad","random","randomseed","strlen","strsub","strlower","strupper","strchar","strrep","ascii","strbyte","format","strfind","gsub","openfile","closefile","date","clock","cgilua","cgilua.lp.translate","cgilua.contentheader","cgilua.script_file","cgilua.header","cgilua.script_path","cgilua.htmlheader","cgilua.script_pdir","cgilua.redirect","cgilua.script_vdir","cgilua.mkabsoluteurl","cgilua.script_vpath","cgilua.mkurlpath","cgilua.servervariable","cgilua.put","cgilua.urlpath","cgilua.handlelp","cgilua.errorlog","cgilua.lp.compile","cgilua.seterrorhandler","cgilua.lp.include","cgilua.seterroroutput","cgilua.lp.setcompatmode","cgilua.addclosefunction","cgilua.lp.setoutfunc","cgilua.addopenfunction","cgilua.addscripthandler","cgilua.addscripthandler","cgilua.buildprocesshandler","cgilua.setmaxfilesize","cgilua.setmaxinput","cgilua.urlcode.encodetable","cgilua.urlcode.escape","cgilua.urlcode.parsequery","cgilua.urlcode.unescape","cgilua.urlcode.insertfield","cgilua.setoutfunc","cgilua.addopenfunction","cgilua.doif","cgilua.doscript","cgilua.pack","cgilua.splitpath","cgilua.cookies.get","cgilua.cookies.set","cgilua.cookies.sethtml","cgilua.cookies.delete","cgilua.serialize","cgilua.session.close","cgilua.session.data","cgilua.session.load","cgilua.session.new","cgilua.session.open","cgilua.session.save","cgilua.session.setsessiondir","cgilua.session.delete","cgilua.session","cgilua.cookies","numrows","connect","close","fetch","getcolnames","getcoltypes","commit","rollback","setautocommit","lfs","lfs.attributes","lfs.chdir","lfs.currentdir","lfs.dir","lfs.lock","lfs.mkdir","lfs.rmdir","lfs.touch","lfs.unlock","zip","zip.open","zip.openfile","files","seek","close","lines"] >>= withAttribute "BFunc"))
                        <|>
                        ((pRegExpr (compileRegex "\\bfunction\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t():!+,-<=>%&*/;?[]^{|}~\\\"" ["and","function","in","local","not","or"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t():!+,-<=>%&*/;?[]^{|}~\\\"" ["nil","false","true"] >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\belse\\b") >>= withAttribute "Control"))
                        <|>
                        ((pRegExpr (compileRegex "\\belseif\\b") >>= withAttribute "Control"))
                        <|>
                        ((pRegExpr (compileRegex "\\bdo\\b") >>= withAttribute "Control"))
                        <|>
                        ((pRegExpr (compileRegex "\\bif\\b") >>= withAttribute "Control"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\b") >>= withAttribute "Control"))
                        <|>
                        ((pKeyword " \n\t():!+,-<=>%&*/;?[]^{|}~\\\"" ["break","do","else","elseif","end","for","if","repeat","return","then","until","while"] >>= withAttribute "Control"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Symbols"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Symbols"))
                        <|>
                        ((pRegExpr (compileRegex "\\b\\d*\\.?\\d*(e|e\\-|e\\+)?\\d+\\b") >>= withAttribute "Numbers"))
                        <|>
                        ((pRegExpr (compileRegex "\\b-?0[xX][0-9a-fA-F]+\\b") >>= withAttribute "Numbers"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[a-zA-Z_][a-zA-Z0-9_]*(?=\\s*([({'\"]|\\[\\[))\\b") >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[A-Z_][A-Z0-9_]*\\b") >>= withAttribute "Constant"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b") >>= withAttribute "Variable"))
                        <|>
                        ((pDetect2Chars False '!' '=' >>= withAttribute "Error"))
                        <|>
                        ((pDetect2Chars False '-' '=' >>= withAttribute "Error"))
                        <|>
                        ((pDetect2Chars False '+' '=' >>= withAttribute "Error"))
                        <|>
                        ((pDetect2Chars False '+' '+' >>= withAttribute "Error"))
                        <|>
                        ((pDetect2Chars False '.' '=' >>= withAttribute "Error"))
                        <|>
                        ((pAnyChar "[]().=~+-*/^><#;" >>= withAttribute "Symbols")))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetect2Chars False '[' '[' >>= withAttribute "Comment") >>~ pushContext "Block Comment")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Alerts"))
                        <|>
                        ((pKeyword " \n\t():!+,-<=>%&*/;?[]^{|}~\\\"" ["TODO","FIXME","NOTE"] >>= withAttribute "Alerts")))
     return (attr, result)

parseRules "Block Comment" = 
  do (attr, result) <- (((pDetect2Chars False ']' ']' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Alerts"))
                        <|>
                        ((pKeyword " \n\t():!+,-<=>%&*/;?[]^{|}~\\\"" ["TODO","FIXME","NOTE"] >>= withAttribute "Alerts")))
     return (attr, result)

parseRules "String_single" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\(a|b|f|n|r|t|v|\\\\|\"|\\'|[|])") >>= withAttribute "Symbols"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Strings") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "String_double" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\[abfnrtv'\"\\\\\\[\\]]") >>= withAttribute "Symbols"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Strings") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "String_block" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\(a|b|f|n|r|t|v|\\\\|\"|\\'|[|])") >>= withAttribute "Symbols"))
                        <|>
                        ((pDetect2Chars False ']' ']' >>= withAttribute "Strings") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Error" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x
