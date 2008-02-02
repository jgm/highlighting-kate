{- This module was generated from data in the Kate syntax highlighting file coldfusion.xml, version 1.04,
   by   -}

module Text.Highlighting.Kate.Syntax.Coldfusion ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "ColdFusion"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.cfm;*.cfc;*.cfml;*.dbm"

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
  setState $ st { synStLanguage = "ColdFusion" }
  context <- currentContext <|> (pushContext "Normal Text" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("ColdFusion",["Normal Text"])], synStLanguage = "ColdFusion", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal Text" -> return ()
    "ctxCFSCRIPT Tag" -> return ()
    "ctxSCRIPT Tag" -> return ()
    "ctxSTYLE Tag" -> return ()
    "ctxTag" -> return ()
    "ctxTable Tag" -> return ()
    "ctxAnchor Tag" -> return ()
    "ctxImage Tag" -> return ()
    "ctxCF Tag" -> return ()
    "ctxCustom Tag" -> return ()
    "ctxCFX Tag" -> return ()
    "ctxHTML Comment" -> return ()
    "ctxCF Comment" -> return ()
    "ctxC Style Comment" -> return ()
    "ctxOne Line Comment" -> (popContext >> return ())
    "ctxHTML Entities" -> (popContext >> return ())
    "ctxCFSCRIPT Block" -> return ()
    "ctxSCRIPT Block" -> return ()
    "ctxSTYLE Block" -> return ()
    "ctxStyle Properties" -> return ()
    "ctxStyle Values" -> (popContext >> return ())
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }

withAttribute attr txt = do
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Tags","Normal"),("Table Tags","Normal"),("Script Tags","Normal"),("Image Tags","Normal"),("Style Tags","Normal"),("Anchor Tags","Normal"),("Attribute Values","Normal"),("HTML Comment","Comment"),("CF Comment","Comment"),("Script Comment","Comment"),("CF Tags","Normal"),("Custom Tags","Normal"),("CFX Tags","Normal"),("Numbers","Normal"),("HTML Entities","Normal"),("Style Selectors","Normal"),("Style Properties","Normal"),("Style Values","Normal"),("Brackets","Normal"),("Script Numbers","Normal"),("Script Strings","Normal"),("Script Operators","Normal"),("Script Keywords","Normal"),("Script Functions","Function"),("Script Objects","Normal")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("ctxCFSCRIPT Tag","Script Tags"),("ctxSCRIPT Tag","Script Tags"),("ctxSTYLE Tag","Style Tags"),("ctxTag","Tags"),("ctxTable Tag","Table Tags"),("ctxAnchor Tag","Anchor Tags"),("ctxImage Tag","Image Tags"),("ctxCF Tag","CF Tags"),("ctxCustom Tag","Custom Tags"),("ctxCFX Tag","CFX Tags"),("ctxHTML Comment","HTML Comment"),("ctxCF Comment","CF Comment"),("ctxC Style Comment","Script Comment"),("ctxOne Line Comment","Script Comment"),("ctxHTML Entities","HTML Entities"),("ctxCFSCRIPT Block","Normal Text"),("ctxSCRIPT Block","Normal Text"),("ctxSTYLE Block","Style Selectors"),("ctxStyle Properties","Style Properties"),("ctxStyle Values","Style Values")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pString False "<!---" >>= withAttribute "CF Comment") >>~ pushContext "ctxCF Comment")
                        <|>
                        ((pString False "<!--" >>= withAttribute "HTML Comment") >>~ pushContext "ctxHTML Comment")
                        <|>
                        ((pRegExpr (compileRegex "<[cC][fF][sS][cC][rR][iI][pP][tT]") >>= withAttribute "Script Tags") >>~ pushContext "ctxCFSCRIPT Tag")
                        <|>
                        ((pRegExpr (compileRegex "<[sS][cC][rR][iI][pP][tT]") >>= withAttribute "Script Tags") >>~ pushContext "ctxSCRIPT Tag")
                        <|>
                        ((pRegExpr (compileRegex "<[sS][tT][yY][lL][eE]") >>= withAttribute "Style Tags") >>~ pushContext "ctxSTYLE Tag")
                        <|>
                        ((pDetectChar False '&' >>= withAttribute "HTML Entities") >>~ pushContext "ctxHTML Entities")
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[cC][fF]_") >>= withAttribute "Custom Tags") >>~ pushContext "ctxCustom Tag")
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[cC][fF][xX]_") >>= withAttribute "CFX Tags") >>~ pushContext "ctxCFX Tag")
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[cC][fF]") >>= withAttribute "CF Tags") >>~ pushContext "ctxCF Tag")
                        <|>
                        ((pRegExpr (compileRegex "<\\/?([tT][aAhHbBfFrRdD])|([cC][aA][pP][tT])") >>= withAttribute "Table Tags") >>~ pushContext "ctxTable Tag")
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[aA] ") >>= withAttribute "Anchor Tags") >>~ pushContext "ctxAnchor Tag")
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[iI][mM][gG] ") >>= withAttribute "Image Tags") >>~ pushContext "ctxImage Tag")
                        <|>
                        ((pRegExpr (compileRegex "<!?\\/?[a-zA-Z0-9_]+") >>= withAttribute "Tags") >>~ pushContext "ctxTag"))
     return (attr, result)

parseRules "ctxCFSCRIPT Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Script Tags") >>~ pushContext "ctxCFSCRIPT Block")
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxSCRIPT Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Script Tags") >>~ pushContext "ctxSCRIPT Block")
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxSTYLE Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Style Tags") >>~ pushContext "ctxSTYLE Block")
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxTag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxTable Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Table Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxAnchor Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Anchor Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxImage Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Image Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxCF Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "CF Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxCustom Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Custom Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxCFX Tag" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "CFX Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules "ctxHTML Comment" = 
  do (attr, result) <- (((pString False "<!---" >>= withAttribute "CF Comment") >>~ pushContext "ctxCF Comment")
                        <|>
                        ((pString False "-->" >>= withAttribute "HTML Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "ctxCF Comment" = 
  do (attr, result) <- ((pString False "--->" >>= withAttribute "CF Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "ctxC Style Comment" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Script Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "ctxOne Line Comment" = 
  pzero

parseRules "ctxHTML Entities" = 
  do (attr, result) <- ((pDetectChar False ';' >>= withAttribute "HTML Entities") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "ctxCFSCRIPT Block" = 
  do (attr, result) <- (((pDetect2Chars False '/' '*' >>= withAttribute "Script Comment") >>~ pushContext "ctxC Style Comment")
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Script Comment") >>~ pushContext "ctxOne Line Comment")
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Script Strings"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Script Strings"))
                        <|>
                        ((pInt >>= withAttribute "Script Numbers"))
                        <|>
                        ((pFloat >>= withAttribute "Script Numbers"))
                        <|>
                        ((pAnyChar "[()[\\]=+-*/]+" >>= withAttribute "Script Operators"))
                        <|>
                        ((pAnyChar "{}" >>= withAttribute "Brackets"))
                        <|>
                        ((pKeyword ["break","case","catch","continue","default","do","else","for","function","if","in","return","switch","try","var","while"] >>= withAttribute "Script Keywords"))
                        <|>
                        ((pKeyword ["Abs","ACos","ArrayAppend","ArrayAvg","ArrayClear","ArrayDeleteAt","ArrayInsertAt","ArrayIsEmpty","ArrayLen","ArrayMax","ArrayMin","ArrayNew","ArrayPrepend","ArrayResize","ArraySet","ArraySort","ArraySum","ArraySwap","ArrayToList","Asc","ASin","Atn","BitAnd","BitMaskClear","BitMaskRead","BitMaskSet","BitNot","BitOr","BitSHLN","BitSHRN","BitXor","Ceiling","Chr","CJustify","Compare","CompareNoCase","Cos","CreateDate","CreateDateTime","CreateObject","CreateODBCDate","CreateODBCDateTime","CreateODBCTime","CreateTime","CreateTimeSpan","CreateUUID","DateAdd","DateCompare","DateConvert","DateDiff","DateFormat","DatePart","Day","DayOfWeek","DayOfWeekAsString","DayOfYear","DaysInMonth","DaysInYear","DE","DecimalFormat","DecrementValue","Decrypt","DeleteClientVariable","DirectoryExists","DollarFormat","Duplicate","Encrypt","Evaluate","Exp","ExpandPath","FileExists","Find","FindNoCase","FindOneOf","FirstDayOfMonth","Fix","FormatBaseN","GetAuthUser","GetBaseTagData","GetBaseTagList","GetBaseTemplatePath","GetClientVariablesList","GetCurrentTemplatePath","GetDirectoryFromPath","GetException","GetFileFromPath","GetFunctionList","GetHttpRequestData","GetHttpTimeString","GetK2ServerDocCount","GetK2ServerDocCountLimit","GetLocale","GetMetaData","GetMetricData","GetPageContext","GetProfileSections","GetProfileString","GetServiceSettings","GetTempDirectory","GetTempFile","GetTemplatePath","GetTickCount","GetTimeZoneInfo","GetToken","Hash","Hour","HTMLCodeFormat","HTMLEditFormat","IIf","IncrementValue","InputBaseN","Insert","Int","IsArray","IsBinary","IsBoolean","IsCustomFunction","IsDate","IsDebugMode","IsDefined","IsK2ServerABroker","IsK2ServerDocCountExceeded","IsK2ServerOnline","IsLeapYear","IsNumeric","IsNumericDate","IsObject","IsQuery","IsSimpleValue","IsStruct","IsUserInRole","IsWDDX","IsXmlDoc","IsXmlElement","IsXmlRoot","JavaCast","JSStringFormat","LCase","Left","Len","ListAppend","ListChangeDelims","ListContains","ListContainsNoCase","ListDeleteAt","ListFind","ListFindNoCase","ListFirst","ListGetAt","ListInsertAt","ListLast","ListLen","ListPrepend","ListQualify","ListRest","ListSetAt","ListSort","ListToArray","ListValueCount","ListValueCountNoCase","LJustify","Log","Log10","LSCurrencyFormat","LSDateFormat","LSEuroCurrencyFormat","LSIsCurrency","LSIsDate","LSIsNumeric","LSNumberFormat","LSParseCurrency","LSParseDateTime","LSParseEuroCurrency","LSParseNumber","LSTimeFormat","LTrim","Max","Mid","Min","Minute","Month","MonthAsString","Now","NumberFormat","ParagraphFormat","ParameterExists","ParseDateTime","Pi","PreserveSingleQuotes","Quarter","QueryAddColumn","QueryAddRow","QueryNew","QuerySetCell","QuotedValueList","Rand","Randomize","RandRange","REFind","REFindNoCase","RemoveChars","RepeatString","Replace","ReplaceList","ReplaceNoCase","REReplace","REReplaceNoCase","Reverse","Right","RJustify","Round","RTrim","Second","SetEncoding","SetLocale","SetProfileString","SetVariable","Sgn","Sin","SpanExcluding","SpanIncluding","Sqr","StripCR","StructAppend","StructClear","StructCopy","StructCount","StructDelete","StructFind","StructFindKey","StructFindValue","StructGet","StructInsert","StructIsEmpty","StructKeyArray","StructKeyExists","StructKeyList","StructNew","StructSort","StructUpdate","Tan","TimeFormat","ToBase64","ToBinary","ToString","Trim","UCase","URLDecode","URLEncodedFormat","URLSessionFormat","Val","ValueList","Week","WriteOutput","XmlChildPos","XmlElemNew","XmlFormat","XmlNew","XmlParse","XmlSearch","XmlTransform","Year","YesNoFormat"] >>= withAttribute "Script Functions"))
                        <|>
                        ((pRegExpr (compileRegex "</[cC][fF][sS][cC][rR][iI][pP][tT]>") >>= withAttribute "Script Tags") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "ctxSCRIPT Block" = 
  do (attr, result) <- (((pDetect2Chars False '/' '*' >>= withAttribute "Script Comment") >>~ pushContext "ctxC Style Comment")
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Script Comment") >>~ pushContext "ctxOne Line Comment")
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Script Strings"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Script Strings"))
                        <|>
                        ((pInt >>= withAttribute "Script Numbers"))
                        <|>
                        ((pFloat >>= withAttribute "Script Numbers"))
                        <|>
                        ((pAnyChar "[()[\\]=+-*/]+" >>= withAttribute "Script Operators"))
                        <|>
                        ((pAnyChar "{}" >>= withAttribute "Brackets"))
                        <|>
                        ((pKeyword ["if","else","for","in","while","do","continue","break","with","try","catch","switch","case","new","var","function","return","this","delete","true","false","void","throw","typeof","const","default"] >>= withAttribute "Script Keywords"))
                        <|>
                        ((pKeyword ["Anchor","Applet","Area","Array","Boolean","Button","Checkbox","Date","Document","Event","FileUpload","Form","Frame","Function","Hidden","History","Image","Layer","Linke","Location","Math","Navigator","Number","Object","Option","Password","Radio","RegExp","Reset","Screen","Select","String","Submit","Text","Textarea","Window"] >>= withAttribute "Script Objects"))
                        <|>
                        ((pKeyword ["abs","acos","alert","anchor","apply","asin","atan","atan2","back","blur","call","captureEvents","ceil","charAt","charCodeAt","clearInterval","clearTimeout","click","close","compile","concat","confirm","cos","disableExternalCapture","enableExternalCapture","eval","exec","exp","find","floor","focus","forward","fromCharCode","getDate","getDay","getFullYear","getHours","getMilliseconds","getMinutes","getMonth","getSeconds","getSelection","getTime","getTimezoneOffset","getUTCDate","getUTCDay","getUTCFullYear","getUTCHours","getUTCMilliseconds","getUTCMinutes","getUTCMonth","getUTCSeconds","go","handleEvent","home","indexOf","javaEnabled","join","lastIndexOf","link","load","log","match","max","min","moveAbove","moveBelow","moveBy","moveTo","moveToAbsolute","open","parse","plugins.refresh","pop","pow","preference","print","prompt","push","random","releaseEvents","reload","replace","reset","resizeBy","resizeTo","reverse","round","routeEvent","scrollBy","scrollTo","search","select","setDate","setFullYear","setHours","setInterval","setMilliseconds","setMinutes","setMonth","setSeconds","setTime","setTimeout","setUTCDate","setUTCFullYear","setUTCHours","setUTCMilliseconds","setUTCMinutes","setUTCMonth","setUTCSeconds","shift","sin","slice","sort","splice","split","sqrt","stop","String formatting","submit","substr","substring","taintEnabled","tan","test","toLocaleString","toLowerCase","toSource","toString","toUpperCase","toUTCString","unshift","unwatch","UTC","valueOf","watch","write","writeln"] >>= withAttribute "Script Functions"))
                        <|>
                        ((pRegExpr (compileRegex "</[sS][cC][rR][iI][pP][tT]>") >>= withAttribute "Script Tags") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "ctxSTYLE Block" = 
  do (attr, result) <- (((pDetect2Chars False '/' '*' >>= withAttribute "Script Comment") >>~ pushContext "ctxC Style Comment")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Brackets") >>~ pushContext "ctxStyle Properties")
                        <|>
                        ((pRegExpr (compileRegex "</[sS][tT][yY][lL][eE]>") >>= withAttribute "Style Tags") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "ctxStyle Properties" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Brackets") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Script Comment") >>~ pushContext "ctxC Style Comment")
                        <|>
                        ((pDetectChar False ':' >>= withAttribute "Normal Text") >>~ pushContext "ctxStyle Values"))
     return (attr, result)

parseRules "ctxStyle Values" = 
  do (attr, result) <- (((pDetectChar False ';' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False ',' >>= withAttribute "Normal Text"))
                        <|>
                        ((pInt >>= withAttribute "Numbers"))
                        <|>
                        ((pFloat >>= withAttribute "Numbers"))
                        <|>
                        ((pRegExpr (compileRegex "#([0-9a-fA-F]{3})|([0-9a-fA-F]{6})") >>= withAttribute "Numbers"))
                        <|>
                        ((pRegExpr (compileRegex "\"[^\"]*\"") >>= withAttribute "Attribute Values"))
                        <|>
                        ((pRegExpr (compileRegex "'[^']*'") >>= withAttribute "Attribute Values")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x