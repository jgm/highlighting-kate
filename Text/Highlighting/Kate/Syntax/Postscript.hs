{- This module was generated from data in the Kate syntax highlighting file postscript.xml, version 1.01,
   by   -}

module Text.Highlighting.Kate.Syntax.Postscript ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "PostScript"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.ps;*.ai;*.eps"

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
  setState $ st { synStLanguage = "PostScript" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("PostScript",["Normal"])], synStLanguage = "PostScript", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Comment" -> (popContext >> return ())
    "Header" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Comment","Comment"),("Header","Others"),("Float","Float"),("Decimal","DecVal"),("String","String"),("Data Type","DataType")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("Comment","Comment"),("Header","Header"),("String","String")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword ["abs","add","aload","anchorsearch","and","arc","arcn","arct","arcto","array","ashow","astore","awidthshow","begin","bind","bitshift","ceiling","charpath","clear","cleartomark","clip","clippath","closepath","concat","concatmatrix","copy","count","counttomark","currentcmykcolor","currentdash","currentdict","currentfile","currentfont","currentgray","currentgstate","currenthsbcolor","currentlinecap","currentlinejoin","currentlinewidth","currentmatrix","currentpoint","currentrgbcolor","currentshared","curveto","cvi","cvlit","cvn","cvr","cvrs","cvs","cvx","def","defineusername","dict","div","dtransform","dup","end","eoclip","eofill","eoviewclip","eq","exch","exec","exit","file","fill","findfont","flattenpath","floor","flush","flushfile","for","forall","ge","get","getinterval","grestore","gsave","gstate","gt","identmatrix","idiv","idtransform","if","ifelse","image","imagemask","index","ineofill","infill","initviewclip","inueofill","inufill","invertmatrix","itransform","known","le","length","lineto","load","loop","lt","makefont","matrix","maxlength","mod","moveto","mul","ne","neg","newpath","not","null","or","pathbbox","pathforall","pop","print","printobject","put","putinterval","rcurveto","read","readhexstring","readline","readstring","rectclip","rectfill","rectstroke","rectviewclip","repeat","restore","rlineto","rmoveto","roll","rotate","round","save","scale","scalefont","search","selectfont","setbbox","setcachedevice","setcachedevice2","setcharwidth","setcmykcolor","setdash","setfont","setgray","setgstate","sethsbcolor","setlinecap","setlinejoin","setlinewidth","setmatrix","setrgbcolor","setshared","shareddict","show","showpage","stop","stopped","store","string","stringwidth","stroke","strokepath","sub","systemdict","token","transform","translate","truncate","type","uappend","ucache","ueofill","ufill","undef","upath","userdict","ustroke","viewclip","viewclippath","where","widthshow","write","writehexstring","writeobject","writestring","wtranslation","xor","xshow","xyshow","yshow","FontDirectory","SharedFontDirectory","Courier","Courier-Bold","Courier-BoldOblique","Courier-Oblique","Helvetica","Helvetica-Bold","Helvetica-BoldOblique","Helvetica-Oblique","Symbol","Times-Bold","Times-BoldItalic","Times-Italic","Times-Roman","execuserobject","currentcolor","currentcolorspace","currentglobal","execform","filter","findresource","globaldict","makepattern","setcolor","setcolorspace","setglobal","setpagedevice","setpattern","ISOLatin1Encoding","StandardEncoding","atan","banddevice","bytesavailable","cachestatus","closefile","colorimage","condition","copypage","cos","countdictstack","countexecstack","cshow","currentblackgeneration","currentcacheparams","currentcolorscreen","currentcolortransfer","currentcontext","currentflat","currenthalftone","currenthalftonephase","currentmiterlimit","currentobjectformat","currentpacking","currentscreen","currentstrokeadjust","currenttransfer","currentundercolorremoval","defaultmatrix","definefont","deletefile","detach","deviceinfo","dictstack","echo","erasepage","errordict","execstack","executeonly","exp","false","filenameforall","fileposition","fork","framedevice","grestoreall","handleerror","initclip","initgraphics","initmatrix","instroke","inustroke","join","kshow","ln","lock","log","mark","monitor","noaccess","notify","nulldevice","packedarray","quit","rand","rcheck","readonly","realtime","renamefile","renderbands","resetfile","reversepath","rootfont","rrand","run","scheck","setblackgeneration","setcachelimit","setcacheparams","setcolorscreen","setcolortransfer","setfileposition","setflat","sethalftone","sethalftonephase","setmiterlimit","setobjectformat","setpacking","setscreen","setstrokeadjust","settransfer","setucacheparams","setundercolorremoval","sin","sqrt","srand","stack","status","statusdict","true","ucachestatus","undefinefont","usertime","ustrokepath","version","vmreclaim","vmstatus","wait","wcheck","xcheck","yield","defineuserobject","undefineuserobject","UserObjects","cleardictstack","setvmthreshold","currentcolorrendering","currentdevparams","currentoverprint","currentpagedevice","currentsystemparams","currentuserparams","defineresource","findencoding","gcheck","glyphshow","languagelevel","product","pstack","resourceforall","resourcestatus","revision","serialnumber","setcolorrendering","setdevparams","setoverprint","setsystemparams","setuserparams","startjob","undefineresource","GlobalFontDirectory","ASCII85Decode","ASCII85Encode","ASCIIHexDecode","ASCIIHexEncode","CCITTFaxDecode","CCITTFaxEncode","DCTDecode","DCTEncode","LZWDecode","LZWEncode","NullEncode","RunLengthDecode","RunLengthEncode","SubFileDecode","CIEBasedA","CIEBasedABC","DeviceCMYK","DeviceGray","DeviceRGB","Indexed","Pattern","Separation","CIEBasedDEF","CIEBasedDEFG","DeviceN"] >>= withAttribute "Keyword"))
                        <|>
                        ((pDetect2Chars False '%' '!' >>= withAttribute "Header") >>~ pushContext "Header")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pRegExpr (compileRegex "\\/{1,2}[^\\s\\(\\)\\{\\}\\[\\]%/]*") >>= withAttribute "Data Type")))
     return (attr, result)

parseRules "Comment" = 
  pzero

parseRules "Header" = 
  pzero

parseRules "String" = 
  do (attr, result) <- ((pDetectChar False ')' >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x