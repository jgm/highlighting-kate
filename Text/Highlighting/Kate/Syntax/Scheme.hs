{- This module was generated from data in the Kate syntax highlighting file scheme.xml, version 1.12,
   by  Dominik Haumann (dhdev@gmx.de) -}

module Text.Highlighting.Kate.Syntax.Scheme ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Scheme"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.scm;*.ss;*.scheme;*.guile"

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
  setState $ st { synStLanguage = "Scheme" }
  context <- currentContext <|> (pushContext "Level0" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Scheme",["Level0"])], synStLanguage = "Scheme", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.(),%&;[]^{|}~", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Level0" -> return ()
    "Default" -> return ()
    "MultiLineComment" -> return ()
    "SpecialNumber" -> (popContext >> return ())
    "String" -> return ()
    "function_decl" -> return ()
    "Level1" -> return ()
    "Level2" -> return ()
    "Level3" -> return ()
    "Level4" -> return ()
    "Level5" -> return ()
    "Level6" -> return ()
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

styles = [("Normal","Normal"),("Keyword","Keyword"),("Definition","Keyword"),("Operator","Keyword"),("Function","Function"),("Data","DataType"),("Decimal","DecVal"),("BaseN","BaseN"),("Float","Float"),("Char","Char"),("String","String"),("Comment","Comment"),("Region Marker","RegionMarker"),("Brackets1","Normal"),("Brackets2","Normal"),("Brackets3","Normal"),("Brackets4","Normal"),("Brackets5","Normal"),("Brackets6","Normal")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Level0","Normal"),("Default","Normal"),("MultiLineComment","Comment"),("SpecialNumber","Normal"),("String","String"),("function_decl","Function"),("Level1","Normal"),("Level2","Normal"),("Level3","Normal"),("Level4","Normal"),("Level5","Normal"),("Level6","Normal")]

parseRules "Level0" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Brackets1") >>~ pushContext "Level1")
                        <|>
                        ((parseRules "Default")))
     return (attr, result)

parseRules "Default" = 
  do (attr, result) <- (((pRegExpr (compileRegex ";+\\s*BEGIN.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pRegExpr (compileRegex ";+\\s*END.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pRegExpr (compileRegex ";.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pDetect2Chars False '#' '!' >>= withAttribute "Comment") >>~ pushContext "MultiLineComment")
                        <|>
                        ((pKeyword ["abs","acos","and","angle","append","applymap","asin","assoc","assq","assv","atan","begin","boolean?","break","caaaar","caaadr","caaar","caadar","caaddr","caadr","caar","cadaar","cadadr","cadar","caddar","cadddr","caddr","cadr","call/cc","call-with-current-continuation","call-with-input-file","call-with-output-file","call-with-values","car","case","catch","cdaaar","cdaadr","cdaar","cdadar","cdaddr","cdadr","cdar","cddaar","cddadr","cddar","cdddar","cddddr","cdddr","cddr","cdr","ceiling","char-alphabetic?","char-ci>=?","char-ci>?","char-ci=?","char-ci<=?","char-downcase","char->integer","char>=?","char>?","char=?","char?","char-lower-case?","char<?c","char<=?","char-numeric?","char-ready?","char-upcase","char-upper-case?","char-whitespace?","close-input-port","close-output-port","complex?","cond","cons","continue","cos","current-input-port","current-output-port","denominator","display","do","dynamic-wind","else","eof-object?","eq?","equal?","eqv?","eval","even?","exact->inexact","exact?","exp","expt","floor","force","for-each","gcd","har-ci<?","if","imag-part","inexact->exact","inexact?","input-port?","integer->char","integer?","interaction-environment","lambda","lcm","length","let","let*","letrec","letrec-syntax","let-syntax","list->string","list","list?","list-ref","list-tail","load","log","magnitude","make-polar","make-rectangular","make-string","make-vector","max","member","memq","memv","min","modulo","negative?","newline","not","null-environment","null?","number?","number->string","numerator","odd?","open-input-file","open-output-file","or","output-port?","pair?","peek-char","port?","positive?","procedure?","quotient","rational?","rationalize","read-char","read","real?","real-part","remainder","reverse","round","scheme-report-environment","set-car!","set-cdr!","sin","sqrt","string-append","string-ci>=?","string-ci>?","string-ci=?","string-ci<=?","string-ci<?","string-copy","string-fill!","string>=?","string>?","string->list","string->number","string->symbol","string=?","string","string?","string-length","string<=?","string<?","string-ref","string-set!","substring","symbol->string","symbol?","syntax-rules","tan","transcript-off","transcript-on","truncate","values","vector-fill!","vector->listlist->vector","vector","vector?","vector-length","vector-ref","vector-set!","while","with-input-from-file","with-output-to-file","write-char","write","zero?"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["<=","<","=","=>",">=",">","-","/","*,*","*)","+"] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["define","define*","define-accessor","define-class","defined?","define-generic","define-macro","define-method","define-module","define-private","define-public","define*-public","define-reader-ctor","define-syntax","define-syntax-macro","defmacro","defmacro*","defmacro*-public"] >>= withAttribute "Definition") >>~ pushContext "function_decl")
                        <|>
                        ((pKeyword ["#\\nul","#\\soh","#\\stx","#\\etx","#\\eot","#\\enq","#\\ack","#\\bel","#\\bs","#\\ht","#\\nl","#\\vt","#\\np","#\\cr","#\\so","#\\si","#\\dle","#\\dc1","#\\dc2","#\\dc3","#\\dc4","#\\nak","#\\syn","#\\etb","#\\can","#\\em","#\\sub","#\\esc","#\\fs","#\\gs","#\\rs","#\\us","#\\space","#\\sp","#\\newline","#\\nl","#\\tab","#\\ht","#\\backspace","#\\bs","#\\return","#\\cr","#\\page","#\\np","#\\null","#\\nul"] >>= withAttribute "Char"))
                        <|>
                        ((pRegExpr (compileRegex "#\\\\.") >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pRegExpr (compileRegex "#[bodxei]") >>= withAttribute "Char") >>~ pushContext "SpecialNumber")
                        <|>
                        ((pRegExpr (compileRegex "#[tf]") >>= withAttribute "Decimal"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Brackets1") >>~ pushContext "Level1"))
     return (attr, result)

parseRules "MultiLineComment" = 
  do (attr, result) <- ((pColumn 0 >> pRegExpr (compileRegex "!#\\s*$") >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "SpecialNumber" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\d*(\\.\\d+)?") >>= withAttribute "Decimal") >>~ (popContext >> return ()))
                        <|>
                        (return () >> return ([], "")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pKeyword ["#\\nul","#\\soh","#\\stx","#\\etx","#\\eot","#\\enq","#\\ack","#\\bel","#\\bs","#\\ht","#\\nl","#\\vt","#\\np","#\\cr","#\\so","#\\si","#\\dle","#\\dc1","#\\dc2","#\\dc3","#\\dc4","#\\nak","#\\syn","#\\etb","#\\can","#\\em","#\\sub","#\\esc","#\\fs","#\\gs","#\\rs","#\\us","#\\space","#\\sp","#\\newline","#\\nl","#\\tab","#\\ht","#\\backspace","#\\bs","#\\return","#\\cr","#\\page","#\\np","#\\null","#\\nul"] >>= withAttribute "Char"))
                        <|>
                        ((pRegExpr (compileRegex "#\\\\.") >>= withAttribute "Char"))
                        <|>
                        ((pDetect2Chars False '\\' '"' >>= withAttribute "Char"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "function_decl" = 
  do (attr, result) <- ((pRegExpr (compileRegex "\\s*[A-Za-z0-9-+\\<\\>//\\*]*\\s*") >>= withAttribute "Function") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Level1" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Brackets2") >>~ pushContext "Level2")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Brackets1") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Default")))
     return (attr, result)

parseRules "Level2" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Brackets3") >>~ pushContext "Level3")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Brackets2") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Default")))
     return (attr, result)

parseRules "Level3" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Brackets4") >>~ pushContext "Level4")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Brackets3") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Default")))
     return (attr, result)

parseRules "Level4" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Brackets5") >>~ pushContext "Level5")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Brackets4") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Default")))
     return (attr, result)

parseRules "Level5" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Brackets6") >>~ pushContext "Level6")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Brackets5") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Default")))
     return (attr, result)

parseRules "Level6" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Brackets1") >>~ pushContext "Level1")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Brackets6") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "Default")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
