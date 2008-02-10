{- This module was generated from data in the Kate syntax highlighting file javascript.xml, version 1.11,
   by  Anders Lund (anders@alweb.dk), Joseph Wenninger (jowenn@kde.org), Whitehawk Stormchaser (zerokode@gmx.net) -}

module Text.Highlighting.Kate.Syntax.Javascript ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "JavaScript"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.js"

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
  setState $ st { synStLanguage = "JavaScript" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("JavaScript",["Normal"])], synStLanguage = "JavaScript", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String" -> (popContext >> return ())
    "String 1" -> (popContext >> return ())
    "Comment" -> (popContext >> return ())
    "Multi/inline Comment" -> return ()
    "Regular Expression" -> return ()
    "(Internal regex catch)" -> return ()
    "Regular Expression Character Class" -> return ()
    "(regex caret first check)" -> (popContext >> return ())
    "(charclass caret first check)" -> (popContext >> return ())
    "region_marker" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Function","Function"),("Objects","Keyword"),("Math","Keyword"),("Events","Keyword"),("Data Type","DataType"),("Decimal","DecVal"),("Float","Float"),("Char","Char"),("String","String"),("String Char","Char"),("Comment","Comment"),("Symbol","Normal"),("Regular Expression","Others"),("Pattern Internal Operator","Float"),("Pattern Character Class","BaseN"),("Region Marker","RegionMarker")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String","String"),("String 1","String Char"),("Comment","Comment"),("Multi/inline Comment","Comment"),("Regular Expression","Regular Expression"),("(Internal regex catch)","Normal Text"),("Regular Expression Character Class","Pattern Character Class"),("(regex caret first check)","Pattern Internal Operator"),("(charclass caret first check)","Pattern Internal Operator"),("region_marker","Region Marker")]

parseRules "Normal" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pString False "//BEGIN" >>= withAttribute "Region Marker") >>~ pushContext "region_marker")
                        <|>
                        ((pRegExpr (compileRegex "//END") >>= withAttribute "Region Marker") >>~ pushContext "region_marker")
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["if","else","for","in","while","do","continue","break","with","try","catch","finally","switch","case","new","var","function","return","delete","true","false","void","throw","typeof","const","default"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["escape","isFinite","isNaN","Number","parseFloat","parseInt","reload","taint","unescape","untaint","write"] >>= withAttribute "Function"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["Anchor","Applet","Area","Array","Boolean","Button","Checkbox","Date","document","window","Image","FileUpload","Form","Frame","Function","Hidden","Link","MimeType","Math","Max","Min","Layer","navigator","Object","Password","Plugin","Radio","RegExp","Reset","Screen","Select","String","Text","Textarea","this","Window"] >>= withAttribute "Objects"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["abs","acos","asin","atan","atan2","ceil","cos","ctg","E","exp","floor","LN2","LN10","log","LOG2E","LOG10E","PI","pow","round","sin","sqrt","SQRT1_2","SQRT2","tan"] >>= withAttribute "Math"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["onAbort","onBlur","onChange","onClick","onError","onFocus","onLoad","onMouseOut","onMouseOver","onReset","onSelect","onSubmit","onUnload"] >>= withAttribute "Events"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["above","action","alinkColor","alert","anchor","anchors","appCodeName","applets","apply","appName","appVersion","argument","arguments","arity","availHeight","availWidth","back","background","below","bgColor","border","big","blink","blur","bold","border","call","caller","charAt","charCodeAt","checked","clearInterval","clearTimeout","click","clip","close","closed","colorDepth","complete","compile","constructor","confirm","cookie","current","cursor","data","defaultChecked","defaultSelected","defaultStatus","defaultValue","description","disableExternalCapture","domain","elements","embeds","enabledPlugin","enableExternalCapture","encoding","eval","exec","fgColor","filename","find","fixed","focus","fontcolor","fontsize","form","forms","formName","forward","frames","fromCharCode","getDate","getDay","getHours","getMiliseconds","getMinutes","getMonth","getSeconds","getSelection","getTime","getTimezoneOffset","getUTCDate","getUTCDay","getUTCFullYear","getUTCHours","getUTCMilliseconds","getUTCMinutes","getUTCMonth","getUTCSeconds","getYear","global","go","hash","height","history","home","host","hostname","href","hspace","ignoreCase","images","index","indexOf","innerHeight","innerWidth","input","italics","javaEnabled","join","language","lastIndex","lastIndexOf","lastModified","lastParen","layers","layerX","layerY","left","leftContext","length","link","linkColor","links","location","locationbar","load","lowsrc","match","MAX_VALUE","menubar","method","mimeTypes","MIN_VALUE","modifiers","moveAbove","moveBelow","moveBy","moveTo","moveToAbsolute","multiline","name","NaN","NEGATIVE_INFINITY","negative_infinity","next","open","opener","options","outerHeight","outerWidth","pageX","pageY","pageXoffset","pageYoffset","parent","parse","pathname","personalbar","pixelDepth","platform","plugins","pop","port","POSITIVE_INFINITY","positive_infinity","preference","previous","print","prompt","protocol","prototype","push","referrer","refresh","releaseEvents","reload","replace","reset","resizeBy","resizeTo","reverse","rightContext","screenX","screenY","scroll","scrollbar","scrollBy","scrollTo","search","select","selected","selectedIndex","self","setDate","setHours","setMinutes","setMonth","setSeconds","setTime","setTimeout","setUTCDate","setUTCDay","setUTCFullYear","setUTCHours","setUTCMilliseconds","setUTCMinutes","setUTCMonth","setUTCSeconds","setYear","shift","siblingAbove","siblingBelow","small","sort","source","splice","split","src","status","statusbar","strike","sub","submit","substr","substring","suffixes","sup","taintEnabled","target","test","text","title","toGMTString","toLocaleString","toLowerCase","toolbar","toSource","toString","top","toUpperCase","toUTCString","type","URL","unshift","unwatch","userAgent","UTC","value","valueOf","visibility","vlinkColor","vspace","width","watch","which","width","write","writeln","x","y","zIndex"] >>= withAttribute "Data Type"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "String 1")
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Multi/inline Comment")
                        <|>
                        ((pRegExpr (compileRegex "[=?:]") >>= withAttribute "Normal Text") >>~ pushContext "(Internal regex catch)")
                        <|>
                        ((pRegExpr (compileRegex "\\(") >>= withAttribute "Normal Text") >>~ pushContext "(Internal regex catch)")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Symbol"))
                        <|>
                        ((pAnyChar ":!%&+,-/.*<=>?[]|~^;" >>= withAttribute "Symbol")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String"))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "String 1" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String Char") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules "Multi/inline Comment" = 
  do (attr, result) <- (((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Regular Expression" = 
  do (attr, result) <- (((pRegExpr (compileRegex "/[ig]{0,2}") >>= withAttribute "Regular Expression") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\{[\\d, ]+\\}") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[bB]") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[nrtvfDdSsWw]") >>= withAttribute "Pattern Character Class"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Pattern Character Class") >>~ pushContext "(charclass caret first check)")
                        <|>
                        ((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\$(?=/)") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pAnyChar "?+*()|" >>= withAttribute "Pattern Internal Operator")))
     return (attr, result)

parseRules "(Internal regex catch)" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "//(?=;)") >>= withAttribute "Regular Expression") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Multi/inline Comment")
                        <|>
                        ((pDetectChar False '/' >>= withAttribute "Regular Expression") >>~ pushContext "(regex caret first check)")
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Regular Expression Character Class" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\[\\[\\]]") >>= withAttribute "Pattern Character Class"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Pattern Character Class") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "(regex caret first check)" = 
  do (attr, result) <- (((pDetectChar False '^' >>= withAttribute "Pattern Internal Operator") >>~ pushContext "Regular Expression")
                        <|>
                        (pushContext "Regular Expression" >> return ([], "")))
     return (attr, result)

parseRules "(charclass caret first check)" = 
  do (attr, result) <- (((pDetectChar False '^' >>= withAttribute "Pattern Internal Operator") >>~ pushContext "Regular Expression Character Class")
                        <|>
                        (pushContext "Regular Expression Character Class" >> return ([], "")))
     return (attr, result)

parseRules "region_marker" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "Region Marker"))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Region Marker")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
