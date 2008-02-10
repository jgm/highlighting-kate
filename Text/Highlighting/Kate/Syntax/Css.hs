{- This module was generated from data in the Kate syntax highlighting file css.xml, version 2.01,
   by  Wilbert Berendsen (wilbert@kde.nl) -}

module Text.Highlighting.Kate.Syntax.Css ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "CSS"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.css"

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
  setState $ st { synStLanguage = "CSS" }
  context <- currentContext <|> (pushContext "Base" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("CSS",["Base"])], synStLanguage = "CSS", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Base" -> return ()
    "FindRuleSets" -> return ()
    "FindValues" -> return ()
    "FindStrings" -> return ()
    "FindComments" -> return ()
    "Media" -> return ()
    "Media2" -> return ()
    "SelAttr" -> return ()
    "SelPseudo" -> (popContext >> return ())
    "Import" -> return ()
    "Comment" -> return ()
    "RuleSet" -> return ()
    "Rule" -> return ()
    "Rule2" -> return ()
    "PropParen" -> return ()
    "PropParen2" -> return ()
    "StringDQ" -> return ()
    "StringSQ" -> return ()
    "InsideString" -> return ()
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

styles = [("Normal Text","Normal"),("Property","Keyword"),("Unknown Property","Keyword"),("Media","DecVal"),("At Rule","DecVal"),("String","String"),("Value","DataType"),("Important","Keyword"),("Selector Attr","Char"),("Selector Id","Float"),("Selector Class","Float"),("Selector Pseudo","DecVal"),("Comment","Comment"),("Region Marker","RegionMarker"),("Alert","Alert"),("Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Base","Normal Text"),("FindRuleSets","Normal Text"),("FindValues","Normal Text"),("FindStrings","Normal Text"),("FindComments","Normal Text"),("Media","Normal Text"),("Media2","Normal Text"),("SelAttr","Selector Attr"),("SelPseudo","Selector Pseudo"),("Import","Normal Text"),("Comment","Comment"),("RuleSet","Normal Text"),("Rule","Normal Text"),("Rule2","Normal Text"),("PropParen","Normal Text"),("PropParen2","Normal Text"),("StringDQ","String"),("StringSQ","String"),("InsideString","String")]

parseRules "Base" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((parseRules "FindRuleSets")))
     return (attr, result)

parseRules "FindRuleSets" = 
  do (attr, result) <- (((pRegExpr (compileRegex "@media\\b") >>= withAttribute "Media") >>~ pushContext "Media")
                        <|>
                        ((pRegExpr (compileRegex "@import\\b") >>= withAttribute "At Rule") >>~ pushContext "Import")
                        <|>
                        ((pRegExpr (compileRegex "@(font-face|charset)\\b") >>= withAttribute "At Rule"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Property") >>~ pushContext "RuleSet")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Selector Attr") >>~ pushContext "SelAttr")
                        <|>
                        ((pRegExpr (compileRegex "#([a-zA-Z0-9\\-_]|[\\x80-\\xFF]|\\\\[0-9A-Fa-f]{1,6})*") >>= withAttribute "Selector Id"))
                        <|>
                        ((pRegExpr (compileRegex "\\.([a-zA-Z0-9\\-_]|[\\x80-\\xFF]|\\\\[0-9A-Fa-f]{1,6})*") >>= withAttribute "Selector Class"))
                        <|>
                        ((pRegExpr (compileRegex ":lang\\([\\w_-]+\\)") >>= withAttribute "Selector Pseudo"))
                        <|>
                        ((pDetectChar False ':' >>= withAttribute "Selector Pseudo") >>~ pushContext "SelPseudo")
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindComments")))
     return (attr, result)

parseRules "FindValues" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[-+]?[0-9.]+(em|ex|px|in|cm|mm|pt|pc|deg|rad|grad|ms|s|Hz|kHz)\\b") >>= withAttribute "Value"))
                        <|>
                        ((pRegExpr (compileRegex "[-+]?[0-9.]+[%]?") >>= withAttribute "Value"))
                        <|>
                        ((pRegExpr (compileRegex "[\\w\\-]+") >>= withAttribute "Normal Text")))
     return (attr, result)

parseRules "FindStrings" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "StringDQ")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "StringSQ"))
     return (attr, result)

parseRules "FindComments" = 
  do (attr, result) <- (((pRegExpr (compileRegex "/\\*BEGIN.*\\*/") >>= withAttribute "Region Marker"))
                        <|>
                        ((pRegExpr (compileRegex "/\\*END.*\\*/") >>= withAttribute "Region Marker"))
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Comment"))
     return (attr, result)

parseRules "Media" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Media") >>~ pushContext "Media2")
                        <|>
                        ((pKeyword " \n\t.():!+,<=>&*/;?[]^{|}~\\" ["all","aural","braille","embossed","handheld","print","projection","screen","tty","tv"] >>= withAttribute "Media"))
                        <|>
                        ((pDetectChar False ',' >>= withAttribute "Media"))
                        <|>
                        ((parseRules "FindComments"))
                        <|>
                        ((pRegExpr (compileRegex "\\S+") >>= withAttribute "Error")))
     return (attr, result)

parseRules "Media2" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Media") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindRuleSets")))
     return (attr, result)

parseRules "SelAttr" = 
  do (attr, result) <- (((pDetectChar False ']' >>= withAttribute "Selector Attr") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindStrings")))
     return (attr, result)

parseRules "SelPseudo" = 
  do (attr, result) <- (((pKeyword " \n\t.():!+,<=>&*/;?[]^{|}~\\" ["hover","link","visited","active","focus","first-child","last-child","only-child","first-of-type","last-of-type","only-of-type","first-letter","first-line","before","after","selection","root","empty","target","enabled","disabled","checked","indeterminate","nth-child","nth-last-child","nth-of-type","nth-last-of-type","not"] >>= withAttribute "Selector Pseudo") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Import" = 
  do (attr, result) <- (((pDetectChar False ';' >>= withAttribute "At Rule") >>~ (popContext >> return ()))
                        <|>
                        ((pKeyword " \n\t.():!+,<=>&*/;?[]^{|}~\\" ["all","aural","braille","embossed","handheld","print","projection","screen","tty","tv"] >>= withAttribute "Media"))
                        <|>
                        ((parseRules "FindValues"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindComments")))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules "RuleSet" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Property") >>~ (popContext >> return ()))
                        <|>
                        ((pKeyword " \n\t.():!+,<=>&*/;?[]^{|}~\\" ["azimuth","background","background-attachment","background-color","background-image","background-position","background-repeat","border","border-bottom","border-bottom-color","border-bottom-style","border-bottom-width","border-collapse","border-color","border-left","border-left-color","border-left-style","border-left-width","border-right","border-right-color","border-right-style","border-right-width","border-spacing","border-style","border-top","border-top-color","border-top-style","border-top-width","border-width","bottom","caption-side","clear","clip","color","content","counter-increment","counter-reset","cue","cue-after","cue-before","cursor","direction","display","elevation","empty-cells","float","font","font-family","font-size","font-size-adjust","font-stretch","font-style","font-variant","font-weight","height","left","letter-spacing","line-height","list-style","list-style-image","list-style-keyword","list-style-position","list-style-type","margin","margin-bottom","margin-left","margin-right","margin-top","marker-offset","max-height","max-width","min-height","min-width","orphans","outline","outline-color","outline-style","outline-width","overflow","padding","padding-bottom","padding-left","padding-right","padding-top","page","page-break-after","page-break-before","page-break-inside","pause","pause-after","pause-before","pitch","pitch-range","play-during","position","quotes","richness","right","size","speak","speak-header","speak-numeral","speak-punctuation","speech-rate","stress","table-layout","text-align","text-decoration","text-decoration-color","text-indent","text-shadow","text-transform","top","unicode-bidi","vertical-align","visibility","voice-family","volume","white-space","widows","width","word-spacing","z-index","border-radius","box-sizing","opacity","text-shadow","-moz-border-radius","-moz-box-flex","konq_bgpos_x","konq_bgpos_y","font-family","font-size","font-stretch","font-style","font-variant","font-weight","unicode-range","units-per-em","src","panose-1","stemv","stemh","slope","cap-height","x-height","ascent","descent","widths","bbox","definition-src","baseline","centerline","mathline","topline"] >>= withAttribute "Property") >>~ pushContext "Rule")
                        <|>
                        ((pRegExpr (compileRegex "-?[A-Za-z_-]+(?=\\s*:)") >>= withAttribute "Unknown Property") >>~ pushContext "Rule")
                        <|>
                        ((parseRules "FindComments"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "Rule" = 
  do (attr, result) <- (((pDetectChar False ':' >>= withAttribute "Property") >>~ pushContext "Rule2")
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "Rule2" = 
  do (attr, result) <- (((pDetectChar False ';' >>= withAttribute "Property") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Property") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pKeyword " \n\t.():!+,<=>&*/;?[]^{|}~\\" ["inherit","none","hidden","dotted","dashed","solid","double","groove","ridge","inset","outset","xx-small","x-small","small","medium","large","x-large","xx-large","smaller","larger","italic","oblique","small-caps","normal","bold","bolder","lighter","light","100","200","300","400","500","600","700","800","900","transparent","repeat","repeat-x","repeat-y","no-repeat","baseline","sub","super","top","text-top","middle","bottom","text-bottom","left","right","center","justify","konq-center","disc","circle","square","box","decimal","decimal-leading-zero","lower-roman","upper-roman","lower-greek","lower-alpha","lower-latin","upper-alpha","upper-latin","hebrew","armenian","georgian","cjk-ideographic","hiragana","katakana","hiragana-iroha","katakana-iroha","inline","inline-block","block","list-item","run-in","compact","marker","table","inline-table","table-row-group","table-header-group","table-footer-group","table-row","table-column-group","table-column","table-cell","table-caption","auto","crosshair","default","pointer","move","e-resize","ne-resize","nw-resize","n-resize","se-resize","sw-resize","s-resize","w-resize","text","wait","help","above","absolute","always","avoid","below","bidi-override","blink","both","capitalize","caption","close-quote","collapse","condensed","crop","cross","embed","expanded","extra-condensed","extra-expanded","fixed","hand","hide","higher","icon","inside","invert","landscape","level","line-through","loud","lower","lowercase","ltr","menu","message-box","mix","narrower","no-close-quote","no-open-quote","nowrap","open-quote","outside","overline","portrait","pre","pre-line","pre-wrap","relative","rtl","scroll","semi-condensed","semi-expanded","separate","show","small-caption","static","static-position","status-bar","thick","thin","ultra-condensed","ultra-expanded","underline","uppercase","visible","wider","break","serif","sans-serif","cursive","fantasy","monospace","border-box","content-box","-moz-box"] >>= withAttribute "Value"))
                        <|>
                        ((pKeyword " \n\t.():!+,<=>&*/;?[]^{|}~\\" ["aqua","black","blue","fuchsia","gray","green","lime","maroon","navy","olive","purple","red","silver","teal","white","yellow","ActiveBorder","ActiveCaption","AppWorkspace","Background","ButtonFace","ButtonHighlight","ButtonShadow","ButtonText","CaptionText","GrayText","Highlight","HighlightText","InactiveBorder","InactiveCaption","InactiveCaptionText","InfoBackground","InfoText","Menu","MenuText","Scrollbar","ThreeDDarkShadow","ThreeDFace","ThreeDHighlight","ThreeDLightShadow","ThreeDShadow","Window","WindowFrame","WindowText"] >>= withAttribute "Value"))
                        <|>
                        ((pRegExpr (compileRegex "#([0-9A-Fa-f]{3}){1,4}\\b") >>= withAttribute "Value"))
                        <|>
                        ((pKeyword " \n\t.():!+,<=>&*/;?[]^{|}~\\" ["url","attr","rect","rgb","counter","counters","local","format","expression"] >>= withAttribute "Value") >>~ pushContext "PropParen")
                        <|>
                        ((pRegExpr (compileRegex "!important\\b") >>= withAttribute "Important"))
                        <|>
                        ((parseRules "FindValues"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindComments")))
     return (attr, result)

parseRules "PropParen" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Value") >>~ pushContext "PropParen2")
                        <|>
                        ((parseRules "FindComments"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "PropParen2" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Value") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindValues"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindComments")))
     return (attr, result)

parseRules "StringDQ" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "InsideString")))
     return (attr, result)

parseRules "StringSQ" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "InsideString")))
     return (attr, result)

parseRules "InsideString" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\[\"']") >>= withAttribute "String"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "String")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
