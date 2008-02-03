{- This module was generated from data in the Kate syntax highlighting file literate-haskell.xml, version 1.04,
   by  Marcel Martin (mmar@freenet.de) -}

module Text.Highlighting.Kate.Syntax.LiterateHaskell ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Literate Haskell"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.lhs"

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
  setState $ st { synStLanguage = "Literate Haskell" }
  context <- currentContext <|> (pushContext "literate-normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Literate Haskell",["literate-normal"])], synStLanguage = "Literate Haskell", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "literate-normal" -> return ()
    "normal" -> pushContext "literate-normal"
    "comment_single_line" -> (popContext >> return ())
    "comment_multi_line" -> return ()
    "string" -> return ()
    "infix" -> return ()
    "single_char" -> (popContext >> return ())
    "function_definition" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Function","Function"),("Function Definition","Function"),("Class","Keyword"),("Decimal","DecVal"),("Float","Float"),("Char","Char"),("String","String"),("Constructor","Others"),("Comment","Comment"),("Data Constructor","Keyword"),("Type Constructor","DataType"),("Infix Operator","Others"),("Special","Char")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("literate-normal","Comment"),("normal","Normal Text"),("comment_single_line","Comment"),("comment_multi_line","Comment"),("string","String"),("infix","Infix Operator"),("single_char","Char"),("function_definition","Function Definition")]

parseRules "literate-normal" = 
  do (attr, result) <- ((pColumn 0 >> pDetectChar False '>' >>= withAttribute "Special") >>~ pushContext "normal")
     return (attr, result)

parseRules "normal" = 
  do (attr, result) <- (((pDetect2Chars False '{' '-' >>= withAttribute "Comment") >>~ pushContext "comment_multi_line")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "comment_single_line")
                        <|>
                        ((pKeyword ["case","class","data","deriving","do","else","if","in","infixl","infixr","instance","let","module","of","primitive","then","type","where"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["Bounded","Enum","Eq","Floating","Fractional","Functor","Integral","Ix","Monad","Num","Ord","Read","RealFloat","RealFrac","Real","Show"] >>= withAttribute "Class"))
                        <|>
                        ((pKeyword ["Bool","Char","Double","Either","Float","IO","Integer","Int","Maybe","Ordering","Rational","Ratio","ReadS","ShowS","String"] >>= withAttribute "Type Constructor"))
                        <|>
                        ((pKeyword ["FilePath","IOError","abs","acos","acosh","all","and","any","appendFile","approxRational","asTypeOf","asin","asinh","atan","atan2","atanh","basicIORun","break","catch","ceiling","chr","compare","concat","concatMap","const","cos","cosh","curry","cycle","decodeFloat","denominator","digitToInt","div","divMod","drop","dropWhile","either","elem","encodeFloat","enumFrom","enumFromThen","enumFromThenTo","enumFromTo","error","even","exp","exponent","fail","filter","flip","floatDigits","floatRadix","floatRange","floor","fmap","foldl","foldl1","foldr","foldr1","fromDouble","fromEnum","fromInt","fromInteger","fromIntegral","fromRational","fst","gcd","getChar","getContents","getLine","head","id","inRange","index","init","intToDigit","interact","ioError","isAlpha","isAlphaNum","isAscii","isControl","isDenormalized","isDigit","isHexDigit","isIEEE","isInfinite","isLower","isNaN","isNegativeZero","isOctDigit","isPrint","isSpace","isUpper","iterate","last","lcm","length","lex","lexDigits","lexLitChar","lines","log","logBase","lookup","map","mapM","mapM_","max","maxBound","maximum","maybe","min","minBound","minimum","mod","negate","not","notElem","null","numerator","odd","or","ord","otherwise","pi","pred","primExitWith","print","product","properFraction","putChar","putStr","putStrLn","quot","quotRem","range","rangeSize","read","readDec","readFile","readFloat","readHex","readIO","readInt","readList","readLitChar","readLn","readOct","readParen","readSigned","reads","readsPrec","realToFrac","recip","rem","repeat","replicate","return","reverse","round","scaleFloat","scanl","scanl1","scanr","scanr1","seq","sequence","sequence_","show","showChar","showInt","showList","showLitChar","showParen","showSigned","showString","shows","showsPrec","significand","signum","sin","sinh","snd","span","splitAt","sqrt","subtract","succ","sum","tail","take","takeWhile","tan","tanh","threadToIOResult","toEnum","toInt","toInteger","toLower","toRational","toUpper","truncate","uncurry","undefined","unlines","until","unwords","unzip","unzip3","userError","words","writeFile","zip","zip3","zipWith","zipWith3"] >>= withAttribute "Function"))
                        <|>
                        ((pKeyword ["EQ","False","GT","Just","LT","Left","Nothing","Right","True"] >>= withAttribute "Data Constructor"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "string")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Infix Operator") >>~ pushContext "infix")
                        <|>
                        ((pRegExpr (compileRegex "\\w[']+") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Char") >>~ pushContext "single_char")
                        <|>
                        ((pRegExpr (compileRegex "\\s*[a-z_]+\\w*'*\\s*::") >>= withAttribute "Function Definition"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal")))
     return (attr, result)

parseRules "comment_single_line" = 
  pzero

parseRules "comment_multi_line" = 
  do (attr, result) <- ((pDetect2Chars False '-' '}' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "string" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "infix" = 
  do (attr, result) <- ((pDetectChar False '`' >>= withAttribute "Infix Operator") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "single_char" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Char") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "function_definition" = 
  do (attr, result) <- ((pDetectChar False ';' >>= withAttribute "Function Definition") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x