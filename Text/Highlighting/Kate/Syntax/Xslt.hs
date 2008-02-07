{- This module was generated from data in the Kate syntax highlighting file xslt.xml, version 1.03,
   by  Peter Lammich (views@gmx.de) -}

module Text.Highlighting.Kate.Syntax.Xslt ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "xslt"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.xsl;*.xslt"

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
  setState $ st { synStLanguage = "xslt" }
  context <- currentContext <|> (pushContext "normalText" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("xslt",["normalText"])], synStLanguage = "xslt", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.()!+,<=>%&*/;?[]^{|}~\\\"{}", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "normalText" -> return ()
    "detectEntRef" -> return ()
    "tagname" -> return ()
    "attributes" -> return ()
    "attrValue" -> return ()
    "xattributes" -> return ()
    "xattrValue" -> return ()
    "string" -> return ()
    "sqstring" -> return ()
    "comment" -> return ()
    "xpath" -> return ()
    "sqxpath" -> return ()
    "sqxpathstring" -> return ()
    "xpathstring" -> return ()
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

styles = [("Normal Text","Normal"),("Tag","Keyword"),("Attribute","Others"),("Invalid","Error"),("Alert","Alert"),("Attribute Value","String"),("XPath","Others"),("XPath String","String"),("XPath Axis","Keyword"),("XPath/ XSLT Function","Keyword"),("XPath 2.0/ XSLT 2.0 Function","Keyword"),("XPath Attribute","Normal"),("Variable","Normal"),("Comment","Comment"),("XSLT Tag","Keyword"),("XSLT 2.0 Tag","Keyword"),("Entity Reference","DecVal")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("normalText","Normal Text"),("detectEntRef","Normal Text"),("tagname","Tag"),("attributes","Attribute"),("attrValue","Invalid"),("xattributes","Attribute"),("xattrValue","Invalid"),("string","Attribute Value"),("sqstring","Attribute Value"),("comment","Comment"),("xpath","XPath"),("sqxpath","XPath"),("sqxpathstring","XPath String"),("xpathstring","XPath String")]

parseRules "normalText" = 
  do (attr, result) <- (((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "comment")
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Tag") >>~ pushContext "tagname")
                        <|>
                        ((pRegExpr (compileRegex "&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z_:][\\w.:_-]*);") >>= withAttribute "Entity Reference")))
     return (attr, result)

parseRules "detectEntRef" = 
  do (attr, result) <- ((pRegExpr (compileRegex "&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z_:][\\w.:_-]*);") >>= withAttribute "Entity Reference"))
     return (attr, result)

parseRules "tagname" = 
  do (attr, result) <- (((pKeyword ["xsl:value-of","xsl:output","xsl:decimal-format","xsl:apply-templates","xsl:param","xsl:transform","xsl:namespace-alias","xsl:comment","xsl:element","xsl:attribute","xsl:apply-imports","xsl:text","xsl:when","xsl:template","xsl:processing-instruction","xsl:include","xsl:copy-of","xsl:copy","xsl:with-param","xsl:stylesheet","xsl:for-each","xsl:choose","xsl:sort","xsl:otherwise","xsl:key","xsl:variable","xsl:number","xsl:message","xsl:fallback","xsl:strip-space","xsl:import","xsl:preserve-space","xsl:if","xsl:call-template","xsl:attribute-set"] >>= withAttribute "XSLT Tag") >>~ pushContext "xattributes")
                        <|>
                        ((pKeyword ["xsl:perform-sort","xsl:import-schema","xsl:for-each-group","xsl:sequence","xsl:non-matching-substring","xsl:namespace","xsl:next-match","xsl:function","xsl:analyze-string","xsl:output-character","xsl:matching-substring","xsl:result-document","xsl:character-map","xsl:document"] >>= withAttribute "XSLT 2.0 Tag") >>~ pushContext "xattributes")
                        <|>
                        ((pRegExpr (compileRegex "\\s*") >>= withAttribute "Attribute") >>~ pushContext "attributes")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Tag") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "attributes" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Tag") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Tag") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*=\\s*") >>= withAttribute "Normal Text") >>~ pushContext "attrValue"))
     return (attr, result)

parseRules "attrValue" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Invalid") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Invalid") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Attribute Value") >>~ pushContext "string")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Attribute Value") >>~ pushContext "sqstring"))
     return (attr, result)

parseRules "xattributes" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Tag") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Tag") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "select\\s*=\\s*") >>= withAttribute "Attribute") >>~ pushContext "xattrValue")
                        <|>
                        ((pRegExpr (compileRegex "test\\s*=\\s*") >>= withAttribute "Attribute") >>~ pushContext "xattrValue")
                        <|>
                        ((pRegExpr (compileRegex "match\\s*=\\s*") >>= withAttribute "Attribute") >>~ pushContext "xattrValue")
                        <|>
                        ((pRegExpr (compileRegex "\\s*=\\s*") >>= withAttribute "Attribute") >>~ pushContext "attrValue"))
     return (attr, result)

parseRules "xattrValue" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "Invalid") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Invalid") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "XPath") >>~ pushContext "xpath")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "XPath") >>~ pushContext "sqxpath"))
     return (attr, result)

parseRules "string" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "XPath") >>~ pushContext "xpath")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Attribute Value") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "detectEntRef")))
     return (attr, result)

parseRules "sqstring" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "XPath") >>~ pushContext "sqxpath")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Attribute Value") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "detectEntRef")))
     return (attr, result)

parseRules "comment" = 
  do (attr, result) <- (((pString False "-->" >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "-(-(?!->))+") >>= withAttribute "Invalid"))
                        <|>
                        ((pRegExpr (compileRegex "(FIXME|TODO|HACK)") >>= withAttribute "Alert")))
     return (attr, result)

parseRules "xpath" = 
  do (attr, result) <- (((pKeyword ["format-number","position","lang","substring-before","substring","normalize-space","round","translate","starts-with","concat","local-name","key","count","document","system-property","current","boolean","number","contains","name","last","unparsed-entity-uri","sum","generate-id","function-available","element-available","false","substring-after","not","string-length","id","floor","ceiling","namespace-uri","true","string","text"] >>= withAttribute "XPath/ XSLT Function"))
                        <|>
                        ((pKeyword ["zero-or-one","replace","namespace-uri-for-prefix","current-grouping-key","seconds-from-duration","resolve-uri","node-kind","minutes-from-dateTime","implicit-timezone","exactly-one","current-time","current-dateTime","unordered","subtract-dates-yielding-dayTimeDuration","string-join","static-base-uri","months-from-duration","input","exists","default-collation","dateTime","current-group","current-date","collection","timezone-from-time","matches","local-name-from-QName","day-from-date","timezone-from-date","round-half-to-even","month-from-dateTime","month-from-date","hours-from-duration","escape-uri","distinct-values","avg","years-from-duration","unparsed-text","unparsed-entity-public-id","subtract-dateTimes-yielding-dayTimeDuration","subtract-dates-yielding-yearMonthDuration","string-to-codepoints","sequence-node-identical","hours-from-time","hours-from-dateTime","format-time","codepoints-to-string","trace","tokenize","subtract-dateTimes-yielding-yearMonthDuration","subsequence","seconds-from-dateTime","regex-group","one-or-more","node-name","namespace-uri-from-QName","min","idref","format-dateTime","format-date","days-from-duration","compare","base-uri","seconds-from-time","in-scope-prefixes","expanded-QName","adjust-date-to-timezone","year-from-date","resolve-QName","remove","QName","minutes-from-time","max","lower-case","index-of","doc","deep-equal","data","minutes-from-duration","adjust-dateTime-to-timezone","abs","timezone-from-dateTime","reverse","error","ends-with","day-from-dateTime","year-from-dateTime","upper-case","root","normalize-unicode","empty","insert-before","document-uri","adjust-time-to-timezone"] >>= withAttribute "XPath 2.0/ XSLT 2.0 Function"))
                        <|>
                        ((pRegExpr (compileRegex "(ancestor|ancestor-or-self|attribute|child|descendant|descendant-or-self|following|following-sibling|namespace|parent|preceding|preceding-sibling|self)::") >>= withAttribute "XPath Axis"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "XPath") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "XPath String") >>~ pushContext "sqxpathstring")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "XPath") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "@[A-Za-z_:][\\w.:_-]*") >>= withAttribute "XPath Attribute"))
                        <|>
                        ((pRegExpr (compileRegex "\\$[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[A-Za-z_:][\\w.:_-]*") >>= withAttribute "XPath"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Invalid"))
                        <|>
                        ((parseRules "detectEntRef")))
     return (attr, result)

parseRules "sqxpath" = 
  do (attr, result) <- (((pKeyword ["format-number","position","lang","substring-before","substring","normalize-space","round","translate","starts-with","concat","local-name","key","count","document","system-property","current","boolean","number","contains","name","last","unparsed-entity-uri","sum","generate-id","function-available","element-available","false","substring-after","not","string-length","id","floor","ceiling","namespace-uri","true","string","text"] >>= withAttribute "XPath/ XSLT Function"))
                        <|>
                        ((pKeyword ["zero-or-one","replace","namespace-uri-for-prefix","current-grouping-key","seconds-from-duration","resolve-uri","node-kind","minutes-from-dateTime","implicit-timezone","exactly-one","current-time","current-dateTime","unordered","subtract-dates-yielding-dayTimeDuration","string-join","static-base-uri","months-from-duration","input","exists","default-collation","dateTime","current-group","current-date","collection","timezone-from-time","matches","local-name-from-QName","day-from-date","timezone-from-date","round-half-to-even","month-from-dateTime","month-from-date","hours-from-duration","escape-uri","distinct-values","avg","years-from-duration","unparsed-text","unparsed-entity-public-id","subtract-dateTimes-yielding-dayTimeDuration","subtract-dates-yielding-yearMonthDuration","string-to-codepoints","sequence-node-identical","hours-from-time","hours-from-dateTime","format-time","codepoints-to-string","trace","tokenize","subtract-dateTimes-yielding-yearMonthDuration","subsequence","seconds-from-dateTime","regex-group","one-or-more","node-name","namespace-uri-from-QName","min","idref","format-dateTime","format-date","days-from-duration","compare","base-uri","seconds-from-time","in-scope-prefixes","expanded-QName","adjust-date-to-timezone","year-from-date","resolve-QName","remove","QName","minutes-from-time","max","lower-case","index-of","doc","deep-equal","data","minutes-from-duration","adjust-dateTime-to-timezone","abs","timezone-from-dateTime","reverse","error","ends-with","day-from-dateTime","year-from-dateTime","upper-case","root","normalize-unicode","empty","insert-before","document-uri","adjust-time-to-timezone"] >>= withAttribute "XPath 2.0/ XSLT 2.0 Function"))
                        <|>
                        ((pRegExpr (compileRegex "(ancestor|ancestor-or-self|attribute|child|descendant|descendant-or-self|following|following-sibling|namespace|parent|preceding|preceding-sibling|self)::") >>= withAttribute "XPath Axis"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "XPath") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "XPath String") >>~ pushContext "xpathstring")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "XPath") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "@[A-Za-z_:][\\w.:_-]*") >>= withAttribute "XPath Attribute"))
                        <|>
                        ((pRegExpr (compileRegex "\\$[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[A-Za-z_:][\\w.:_-]*") >>= withAttribute "XPath"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Invalid"))
                        <|>
                        ((parseRules "detectEntRef")))
     return (attr, result)

parseRules "sqxpathstring" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "XPath String") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "detectEntRef")))
     return (attr, result)

parseRules "xpathstring" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "XPath String") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "detectEntRef")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
