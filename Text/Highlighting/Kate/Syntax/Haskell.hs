{- This module was generated from data in the Kate syntax highlighting file haskell.xml, version 1.04,
   by  Marcel Martin (mmar@freenet.de) -}

module Text.Highlighting.Kate.Syntax.Haskell ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import qualified Data.Set as Set
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Haskell"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.hs"

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
  setState $ st { synStLanguage = "Haskell" }
  context <- currentContext <|> (pushContext "normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Haskell",["normal"])], synStLanguage = "Haskell", synStCurrentLine = "", synStCharsParsedInLine = 0, synStPrevChar = '\n', synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "normal" -> return ()
    "comment_single_line" -> (popContext >> return ())
    "comment_multi_line" -> return ()
    "string" -> return ()
    "infix" -> return ()
    "single_char" -> (popContext >> return ())
    "function_definition" -> (popContext >> return ())
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0, synStPrevChar = '\n' }

withAttribute attr txt = do
  if null txt
     then fail "Parser matched no text"
     else return ()
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  let prevchar = if null txt then '\n' else last txt
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt, synStPrevChar = prevchar } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Module Name","Normal"),("Keyword","Keyword"),("Function","Function"),("Function Definition","Function"),("Class","Keyword"),("Decimal","DecVal"),("Float","Float"),("Char","Char"),("String","String"),("Constructor","Others"),("Comment","Comment"),("Data Constructor","Keyword"),("Type Constructor","DataType"),("Infix Operator","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

list26a60b534a1ab03a6d626f2a0ae5be15957ab23e = Set.fromList $ words $ "as case class data deriving do else if import in infixl infixr instance let module of primitive qualified then type where"
list3b59bd0ef39487d2ddb0869a1f7c083cb363d74b = Set.fromList $ words $ "quot rem div mod elem notElem seq"
list466c48cb210ecb8c9ed1920e168bfbf120780b1f = Set.fromList $ words $ "FilePath IOError abs acos acosh all and any appendFile approxRational asTypeOf asin asinh atan atan2 atanh basicIORun break catch ceiling chr compare concat concatMap const cos cosh curry cycle decodeFloat denominator digitToInt div divMod drop dropWhile either elem encodeFloat enumFrom enumFromThen enumFromThenTo enumFromTo error even exp exponent fail filter flip floatDigits floatRadix floatRange floor fmap foldl foldl1 foldr foldr1 fromDouble fromEnum fromInt fromInteger fromIntegral fromRational fst gcd getChar getContents getLine head id inRange index init intToDigit interact ioError isAlpha isAlphaNum isAscii isControl isDenormalized isDigit isHexDigit isIEEE isInfinite isLower isNaN isNegativeZero isOctDigit isPrint isSpace isUpper iterate last lcm length lex lexDigits lexLitChar lines log logBase lookup map mapM mapM_ max maxBound maximum maybe min minBound minimum mod negate not notElem null numerator odd or ord otherwise pi pred primExitWith print product properFraction putChar putStr putStrLn quot quotRem range rangeSize read readDec readFile readFloat readHex readIO readInt readList readLitChar readLn readOct readParen readSigned reads readsPrec realToFrac recip rem repeat replicate return reverse round scaleFloat scanl scanl1 scanr scanr1 seq sequence sequence_ show showChar showInt showList showLitChar showParen showSigned showString shows showsPrec significand signum sin sinh snd span splitAt sqrt subtract succ sum tail take takeWhile tan tanh threadToIOResult toEnum toInt toInteger toLower toRational toUpper truncate uncurry undefined unlines until unwords unzip unzip3 userError words writeFile zip zip3 zipWith zipWith3"
list0a5cedcddf8557b6461fbaa691d87219c2f770d8 = Set.fromList $ words $ "Bool Char Double Either Float IO Integer Int Maybe Ordering Rational Ratio ReadS ShowS String"
listf2d816ea85ab93cd81f87ed9429d56e9ae0d291e = Set.fromList $ words $ "Bounded Enum Eq Floating Fractional Functor Integral Ix Monad Num Ord Read RealFloat RealFrac Real Show"
liste7e30f7b0946ce567de8ccf1256e396cf1011e83 = Set.fromList $ words $ "EQ False GT Just LT Left Nothing Right True"

defaultAttributes = [("normal","Normal Text"),("comment_single_line","Comment"),("comment_multi_line","Comment"),("string","String"),("infix","Infix Operator"),("single_char","Char"),("function_definition","Function Definition")]

parseRules "normal" = 
  do (attr, result) <- (((pDetect2Chars False '{' '-' >>= withAttribute "Comment") >>~ pushContext "comment_multi_line")
                        <|>
                        ((pRegExpr (compileRegex "--$") >>= withAttribute "Comment") >>~ pushContext "comment_single_line")
                        <|>
                        ((pRegExpr (compileRegex "--[ A-Za-z0-9\\-,;`].*$") >>= withAttribute "Comment") >>~ pushContext "comment_single_line")
                        <|>
                        ((pRegExpr (compileRegex "([A-Z][A-Za-z0-9]*\\.)+[A-Z][A-Za-z0-9]*") >>= withAttribute "Module Name"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" list26a60b534a1ab03a6d626f2a0ae5be15957ab23e >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" listf2d816ea85ab93cd81f87ed9429d56e9ae0d291e >>= withAttribute "Class"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" list0a5cedcddf8557b6461fbaa691d87219c2f770d8 >>= withAttribute "Type Constructor"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" list466c48cb210ecb8c9ed1920e168bfbf120780b1f >>= withAttribute "Function"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" liste7e30f7b0946ce567de8ccf1256e396cf1011e83 >>= withAttribute "Data Constructor"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "string")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Infix Operator") >>~ pushContext "infix")
                        <|>
                        ((pRegExpr (compileRegex "\\w[']+") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Char") >>~ pushContext "single_char")
                        <|>
                        ((pRegExpr (compileRegex "[a-z_]+\\w*'*\\s*::") >>= withAttribute "Function Definition"))
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
