module Text.Highlighting.Kate.Common where
import Text.Regex.PCRE.Light.Char8
import Text.Highlighting.Kate.Definitions
import Text.ParserCombinators.Parsec
import Data.Char (toUpper, isDigit, chr)
import qualified Data.Map as Map

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x

capitalize [] = []
capitalize (a:as) = toUpper a : as

normalizeHighlighting :: [LabeledSource] -> [LabeledSource]
normalizeHighlighting [] = []
normalizeHighlighting ((a,x):(b,y):xs) | a == b = normalizeHighlighting ((a, x++y):xs)
normalizeHighlighting (x:xs) = x : normalizeHighlighting xs

pushContext context = if context == "#stay"
                         then return ()
                         else do st <- getState
                                 let contexts = synStContexts st
                                 let lang = synStLanguage st
                                 let addContext c x = case x of
                                                       Nothing -> Just [c]
                                                       Just cs -> Just (c:cs)
                                 let newContexts = Map.alter (addContext context) lang contexts 
                                 updateState $ \st -> st { synStContexts = newContexts }

popContext = do st <- getState
                let contexts = synStContexts st
                let lang = synStLanguage st
                case Map.lookup lang contexts of
                    Just conts -> case length conts of
                                        0 -> fail $ "Stack empty for language " ++ lang
                                        1 -> return (head conts) -- don't remove last member of stack
                                        _ -> do let newContexts = Map.adjust tail lang contexts
                                                updateState $ \st -> st { synStContexts = newContexts }
                                                return (head conts)
                    Nothing    -> fail $ "No context stack for language " ++ lang 

currentContext = do st <- getState
                    let contexts = synStContexts st
                    let lang = synStLanguage st
                    case Map.lookup lang contexts of
                         Just conts -> if length conts < 1
                                          then fail $ "Stack empty for language " ++ lang
                                          else return (head conts)
                         Nothing    -> fail $ "No context stack for language " ++ lang

withChildren parent child = do
  (pAttr, pResult) <- parent
  (_, cResult) <- option ([],"") child
  return (pAttr, pResult ++ cResult)

wholeLine = manyTill anyChar (newline <|> (eof >> return '\n'))

pFirstNonSpace = do
  curLine <- currentLine
  charsParsedInLine <- getState >>= return . synStCharsParsedInLine
  let (sps, nonSps) = span (`elem` " \t") curLine
  if length sps == charsParsedInLine && length nonSps > 0 
     then return ()
     else fail "Not first nonspace"

currentColumn = getPosition >>= return . sourceColumn

currentLine = getState >>= return . synStCurrentLine

pColumn col = do
  curCol <- currentColumn
  if col == (curCol - 1) -- parsec's columns start with 1
     then return ()
     else fail $ "Not column " ++ show col

pGetCapture capNum = do
  captures <- getState >>= return . synStCaptures
  if length captures < capNum
     then fail "Not enough captures"
     else return $ captures !! (capNum - 1)

pDetectChar dynamic ch = do 
  if dynamic && isDigit ch
     then pGetCapture (read [ch]) >>= try . string
     else char ch >>= return . (:[])

pDetect2Chars dynamic ch1 ch2 = try $ do
  [c1] <- pDetectChar dynamic ch1
  [c2] <- pDetectChar dynamic ch2
  return [c1, c2]

pKeyword list = try $ do
  st <- getState
  let caseSensitive = synStKeywordCaseSensitive st
  let delims = synStKeywordDelims st
  let curLine = synStCurrentLine st
  let charsParsed = synStCharsParsedInLine st
  let prevChar = if charsParsed == 0
                    then Nothing
                    else if length curLine < charsParsed
                            then Nothing
                            else Just $ curLine !! (charsParsed - 1)
  case prevChar of
         Just x | not (x `elem` delims) -> fail "Not preceded by a delimiter"
         _ -> return ()
  word <- many1 (noneOf delims)
  if word `elem` list
     then return word
     else if not caseSensitive && (map toUpper word) `elem` (map (map toUpper) list)
             then return word
             else fail "Keyword not in list"

pString dynamic str =
  if dynamic
     then subDynamic str >>= try . string
     else try $ string str

pAnyChar chars = oneOf chars >>= return . (:[])

pDefault = noneOf "\n" >>= return . (:[])

-- The following alternative gives a 25% speed improvement, but it's possible
-- that it won't work for all syntaxes:
-- pDefault = (many1 alphaNum) <|> (noneOf "\n" >>= return . (:[]))

subDynamic ('%':x:xs) | isDigit x = do
  captures <- getState >>= return . synStCaptures
  let capNum = read [x]
  let replacement = if length captures < capNum
                       then ['%',x]
                       else captures !! (capNum - 1)
  subDynamic xs >>= return . (replacement ++)
subDynamic (x:xs) = subDynamic xs >>= return . (x:)
subDynamic "" = return ""

pRegExpr compiledRegex = do
  st <- getState
  let curLine = synStCurrentLine st
  let charsParsedInLine = synStCharsParsedInLine st
  -- Note: we keep one preceding character, so initial \b can match or not...
  let remaining = if charsParsedInLine == 0
                     then ' ':curLine
                     else drop (charsParsedInLine - 1) curLine 
  case match compiledRegex remaining [exec_notempty] of
        Just (x:xs) -> do if null xs
                             then return ()
                             else updateState (\st -> st {synStCaptures = xs})
                          string (drop 1 x) 
        _           -> fail $ "Regex " ++ (show compiledRegex) ++ " failed to match"

pRegExprDynamic regexpStr = do
  regexpStr' <- subDynamic regexpStr
  let compiledRegex = compileRegex regexpStr'
  pRegExpr compiledRegex 

escapeRegex :: String -> String
escapeRegex [] = ""
escapeRegex ('\\':'0':x:y:z:rest) | isDigit x && isDigit y && isDigit z =
  chr (read ['0','o',x,y,z]) : escapeRegex rest
escapeRegex ('\\':x:y:z:rest) | isDigit x && isDigit y && isDigit z =
  chr (read ['0','o',x,y,z]) : escapeRegex rest
escapeRegex (x:xs) = x : escapeRegex xs 

compileRegex regexpStr = compile ('.' : escapeRegex regexpStr) [anchored]

integerRegex = compileRegex "\\b[-+]?(0[Xx][0-9A-Fa-f]+|0[Oo][0-7]+|[0-9]+)\\b"

pInt = pRegExpr integerRegex 

pUnimplemented :: GenParser Char st [Char]
pUnimplemented = do
  fail "Not implemented"
  return ""

floatRegex = compileRegex "\\b[-+]?(([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+)\\b"

pFloat = pRegExpr floatRegex

octRegex = compileRegex "\\b[-+]?0[Oo][0-7]+\\b"

pHlCOct = pRegExpr octRegex

hexRegex = compileRegex "\\b[-+]?0[Xx][0-9A-Fa-f]+\\b"

pHlCHex = pRegExpr hexRegex

pHlCStringChar = try $ do 
  char '\\'
  (oneOf "abefnrtv\"'?\\" >>= return  . (\x -> ['\\',x]))
    <|> (do a <- oneOf "xX"
            b <- many1 hexDigit
            return ('\\':a:b))
    <|> (do a <- char '0'
            b <- many1 octDigit
            return ('\\':a:b))

pHlCChar = try $ do
  char '\''
  c <- pHlCStringChar
  char '\''
  return ('\'' : c ++ "'")

pRangeDetect startChar endChar = try $ do
  char startChar
  body <- manyTill (noneOf ['\n', endChar]) (char endChar)
  return $ startChar : (body ++ [endChar])

pLineContinue = try $ string "\\\n"

pDetectSpaces = many1 (oneOf "\t ")

pDetectIdentifier = do
  first <- letter
  rest <- many alphaNum
  return (first:rest)

