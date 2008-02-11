{- This module was generated from data in the Kate syntax highlighting file tcl.xml, version 1.10,
   by   -}

module Text.Highlighting.Kate.Syntax.Tcl ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Tcl/Tk"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.tcl;*.tk"

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
  setState $ st { synStLanguage = "Tcl/Tk" }
  context <- currentContext <|> (pushContext "Base" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Tcl/Tk",["Base"])], synStLanguage = "Tcl/Tk", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Base" -> return ()
    "String" -> return ()
    "Comment" -> (popContext >> return ())
    "New command line" -> return ()
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Decimal","DecVal"),("Float","Float"),("String","String"),("Comment","Comment"),("Parameter","Others"),("Variable","DataType"),("Char","Char"),("Region Marker","RegionMarker")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Base","Normal Text"),("String","String"),("Comment","Comment"),("New command line","Normal Text")]

parseRules "Base" = 
  do (attr, result) <- (((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*BEGIN.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*END.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["after","append","AppleScript","argv","argc","array","auto_execk","auto_load","auto_mkindex","auto_path","auto_reset","beep","bell","binary","bind","bindtags","bgerror","break","button","canvas","case","catch","cd","checkbutton","clipboard","clock","close","concat","console","continue","dde","destroy","else","elseif","encoding","entry","env","eof","error","errorCode","errorInfo","eval","event","exec","exit","expr","fblocked","fconfigure","fcopy","file","fileevent","flush","focus","font","for","foreach","format","frame","gets","glob","global","grab","grid","history","if","image","incr","info","interp","join","label","lappend","lindex","linsert","list","listbox","llength","load","lower","lrange","lreplace","lsearch","lsort","menu","menubutton","message","namespace","open","option","OptProc","pack","package","parray","pid","place","pkg_mkindex","proc","puts","pwd","radiobutton","raise","read","regexp","registry","regsub","rename","resource","return","scale","scan","scrollbar","seek","selection","send","set","socket","source","split","string","subst","switch","tclLog","tcl_endOfWord","tcl_findLibrary","tcl_library","tcl_patchLevel","tcl_platform","tcl_precision","tcl_rcFileName","tcl_rcRsrcName","tcl_startOfNextWord","tcl_startOfPreviousWord","tcl_traceCompile","tcl_traceExec","tcl_version","tcl_wordBreakAfter","tcl_wordBreakBefore","tell","text","time","tk","tkTabToWindow","tkwait","tk_chooseColor","tk_chooseDirectory","tk_focusFollowMouse","tk_focusNext","tk_focusPrev","tk_getOpenFile","tk_getSaveFile","tk_library","tk_messageBox","tk_optionMenu","tk_patchLevel","tk_popup","tk_strictMotif","tk_version","toplevel","trace","unknown","unset","update","uplevel","upvar","variable","vwait","while","winfo","wm"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["add","args","atime","attributes","body","bytelength","cancel","channels","clicks","cmdcount","commands","compare","complete","convertfrom","convertto","copy","default","delete","dirname","equal","executable","exists","extension","first","forget","format","functions","globals","hostname","idle","ifneeded","index","info","is","isdirectory","isfile","join","last","length","level","library","link","loaded","locals","lstat","map","match","mkdir","mtime","nameofexecutable","names","nativename","normalize","number","owned","patchlevel","pathtype","present","procs","provide","range","readable","readlink","remove","rename","repeat","replace","require","rootname","scan","script","seconds","separator","sharedlibextension","size","split","stat","system","tail","tclversion","tolower","totitle","toupper","trim","trimleft","trimright","type","unknown","variable","vars","vcompare","vdelete","versions","vinfo","volumes","vsatisfies","wordend","wordstart","writable","activate","actual","addtag","append","appname","aspect","atom","atomname","bbox","bind","broadcast","canvasx","canvasy","caret","cells","cget","children","class","clear","client","clone","colormapfull","colormapwindows","command","configure","containing","coords","create","current","curselection","dchars","debug","deiconify","delta","depth","deselect","dlineinfo","dtag","dump","edit","entrycget","entryconfigure","families","find","flash","focus","focusmodel","fpixels","fraction","frame","generate","geometry","get","gettags","grid","group","handle","height","hide","iconbitmap","iconify","iconmask","iconname","iconposition","iconwindow","icursor","id","identify","image","insert","interps","inuse","invoke","ismapped","itemcget","itemconfigure","keys","lower","manager","mark","maxsize","measure","metrics","minsize","move","name","nearest","overrideredirect","own","panecget","paneconfigure","panes","parent","pathname","pixels","pointerx","pointerxy","pointery","positionfrom","post","postcascade","postscript","protocol","proxy","raise","release","reqheight","reqwidth","resizable","rgb","rootx","rooty","scale","scaling","screen","screencells","screendepth","screenheight","screenmmheight","screenmmwidth","screenvisual","screenwidth","search","see","select","selection","server","set","show","sizefrom","stackorder","state","status","tag","title","toplevel","transient","types","unpost","useinputmethods","validate","values","viewable","visual","visualid","visualsavailable","vrootheight","vrootwidth","vrootx","vrooty","width","window","windowingsystem","withdraw","x","xview","y"] >>= withAttribute "Parameter"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Char"))
                        <|>
                        ((pRegExpr (compileRegex "\\s-\\w+") >>= withAttribute "Parameter"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{([^\\}]|\\\\\\})+\\}") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$(::|\\w)+") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\"{2}") >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "\"") >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetectChar False ';' >>= withAttribute "Normal Text") >>~ pushContext "New command line")
                        <|>
                        ((pFirstNonSpace >> pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Keyword"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Keyword"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Keyword"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Keyword")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Variable")))
     return (attr, result)

parseRules "Comment" = 
  pzero

parseRules "New command line" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*#") >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((lookAhead (pRegExpr (compileRegex ".")) >> return ([],"") ) >>~ (popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
