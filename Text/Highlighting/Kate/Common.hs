{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Highlighting.Kate.Common
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Parsers used in all the individual syntax parsers.
-}

module Text.Highlighting.Kate.Common where
#ifdef _PCRE_LIGHT
import Text.Regex.PCRE.Light.Char8
#else
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.String
#endif
import Text.Highlighting.Kate.Definitions
import Text.ParserCombinators.Parsec
import Data.Char (isDigit, chr, toLower)
import Data.List (tails)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Match filename against a list of globs contained in a semicolon-separated
-- string.
matchGlobs :: String -> String -> Bool
matchGlobs fn globs = any (flip matchGlob fn) (splitBySemi $ filter (/=' ') globs)

-- | Match filename against a glob pattern with asterisks.
matchGlob :: String -> String -> Bool
matchGlob ('*':xs) fn = any (matchGlob xs) (tails fn)
matchGlob (x:xs) (y:ys) = x == y && matchGlob xs ys
matchGlob "" "" = True
matchGlob _ _   = False

-- | Splits semicolon-separated list
splitBySemi :: String -> [String]
splitBySemi "" = []
splitBySemi xs =
  let (pref, suff) = break (==';') xs
  in  case suff of
         []       -> [pref]
         (';':ys) -> pref : splitBySemi ys
         _        -> error $ "The impossible happened (splitBySemi)"

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x

normalizeHighlighting :: [LabeledSource] -> [LabeledSource]
normalizeHighlighting [] = []
normalizeHighlighting ((_,""):xs) = normalizeHighlighting xs
normalizeHighlighting ((a,x):(b,y):xs) | a == b = normalizeHighlighting ((a, x++y):xs)
normalizeHighlighting (x:xs) = x : normalizeHighlighting xs

pushContext :: [Char] -> GenParser tok SyntaxState ()
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

popContext :: GenParser tok SyntaxState String
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

currentContext :: GenParser tok SyntaxState String
currentContext = do st <- getState
                    let contexts = synStContexts st
                    let lang = synStLanguage st
                    case Map.lookup lang contexts of
                         Just conts -> if length conts < 1
                                          then fail $ "Stack empty for language " ++ lang
                                          else return (head conts)
                         Nothing    -> fail $ "No context stack for language " ++ lang

withChildren :: GenParser tok SyntaxState LabeledSource
             -> GenParser tok SyntaxState LabeledSource
             -> GenParser tok SyntaxState LabeledSource
withChildren parent child = do
  (pAttr, pResult) <- parent
  (_, cResult) <- option ([],"") child
  return (pAttr, pResult ++ cResult)

wholeLine :: GenParser Char st [Char]
wholeLine = manyTill anyChar (newline <|> (eof >> return '\n'))

pFirstNonSpace :: GenParser tok SyntaxState ()
pFirstNonSpace = do
  curLine <- currentLine
  charsParsedInLine <- getState >>= return . synStCharsParsedInLine
  let (sps, nonSps) = span (`elem` " \t") curLine
  if length sps == charsParsedInLine && length nonSps > 0 
     then return ()
     else fail "Not first nonspace"

currentColumn :: GenParser tok st Column
currentColumn = getPosition >>= return . sourceColumn

currentLine :: GenParser tok SyntaxState String
currentLine = getState >>= return . synStCurrentLine

pColumn :: Column -> GenParser tok st ()
pColumn col = do
  curCol <- currentColumn
  if col == (curCol - 1) -- parsec's columns start with 1
     then return ()
     else fail $ "Not column " ++ show col

pGetCapture :: Int -> GenParser tok SyntaxState String
pGetCapture capNum = do
  captures <- getState >>= return . synStCaptures
  if length captures < capNum
     then fail "Not enough captures"
     else return $ captures !! (capNum - 1)

pDetectChar :: Bool -> Char -> GenParser Char SyntaxState String
pDetectChar dynamic ch = do 
  if dynamic && isDigit ch
     then pGetCapture (read [ch]) >>= try . string
     else char ch >>= return . (:[])

pDetect2Chars :: Bool -> Char -> Char -> GenParser Char SyntaxState [Char]
pDetect2Chars dynamic ch1 ch2 = try $ do
  [c1] <- pDetectChar dynamic ch1
  [c2] <- pDetectChar dynamic ch2
  return [c1, c2]

pKeyword :: [Char] -> Set.Set [Char] -> GenParser Char SyntaxState [Char]
pKeyword delims kws = try $ do
  st <- getState
  let prevChar = synStPrevChar st
  case prevChar of
         x | not (x `elem` delims) -> fail "Not preceded by a delimiter"
         _ -> return ()
  word <- many1 (noneOf delims)
  let word' = if synStKeywordCaseSensitive st
                 then word
                 else map toLower word
  if word' `Set.member` kws
     then return word
     else fail "Keyword not in list"

pString :: Bool -> [Char] -> GenParser Char SyntaxState String
pString dynamic str =
  if dynamic
     then subDynamic str >>= try . string
     else try $ string str

pAnyChar :: [Char] -> GenParser Char st [Char]
pAnyChar chars = oneOf chars >>= return . (:[])

pDefault :: GenParser Char st [Char]
pDefault = noneOf "\n" >>= return . (:[])

-- The following alternative gives a 25% speed improvement, but it's possible
-- that it won't work for all syntaxes:
-- pDefault = (many1 alphaNum) <|> (noneOf "\n" >>= return . (:[]))

subDynamic :: [Char] -> GenParser tok SyntaxState [Char]
subDynamic ('%':x:xs) | isDigit x = do
  captures <- getState >>= return . synStCaptures
  let capNum = read [x]
  let replacement = if length captures < capNum
                       then ['%',x]
                       else captures !! (capNum - 1)
  subDynamic xs >>= return . (replacement ++)
subDynamic (x:xs) = subDynamic xs >>= return . (x:)
subDynamic "" = return ""

compileRegex :: String -> Regex
#ifdef _PCRE_LIGHT
compileRegex regexpStr = compile ('.' : escapeRegex regexpStr) [anchored]
#else
compileRegex regexpStr =
  case unsafePerformIO $ compile (compAnchored) (execNotEmpty) ('.' : escapeRegex regexpStr) of
        Left _ -> error $ "Error compiling regex: " ++ show regexpStr
        Right r -> r
#endif

matchRegex :: Regex -> String -> Maybe [String]
#ifdef _PCRE_LIGHT
matchRegex r s = match r s [exec_notempty] 
#else
matchRegex r s = case unsafePerformIO (regexec r s) of 
                      Right (Just (_, mat, _ , capts)) -> Just (mat : capts)
                      Right Nothing -> Nothing
                      Left matchError -> error $ show matchError
#endif

pRegExpr :: Regex -> GenParser Char SyntaxState String
pRegExpr compiledRegex = do
  st <- getState
  let curLine = synStCurrentLine st
  let charsParsedInLine = synStCharsParsedInLine st
  -- Note: we keep one preceding character, so initial \b can match or not...
  let remaining = if charsParsedInLine == 0
                     then ' ':curLine
                     else drop (charsParsedInLine - 1) curLine 
  case matchRegex compiledRegex remaining of
        Just (x:xs) -> do if null xs
                             then return ()
                             else updateState (\st -> st {synStCaptures = xs})
                          string (drop 1 x) 
        _           -> pzero

pRegExprDynamic :: [Char] -> GenParser Char SyntaxState String
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

integerRegex :: Regex
integerRegex = compileRegex "\\b[-+]?(0[Xx][0-9A-Fa-f]+|0[Oo][0-7]+|[0-9]+)\\b"

pInt :: GenParser Char SyntaxState String
pInt = pRegExpr integerRegex 

floatRegex :: Regex
floatRegex = compileRegex "\\b[-+]?(([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+)\\b"

pFloat :: GenParser Char SyntaxState String
pFloat = pRegExpr floatRegex

octRegex :: Regex
octRegex = compileRegex "\\b[-+]?0[Oo][0-7]+\\b"

pHlCOct :: GenParser Char SyntaxState String
pHlCOct = pRegExpr octRegex

hexRegex :: Regex
hexRegex = compileRegex "\\b[-+]?0[Xx][0-9A-Fa-f]+\\b"

pHlCHex :: GenParser Char SyntaxState String
pHlCHex = pRegExpr hexRegex

pHlCStringChar :: GenParser Char st [Char]
pHlCStringChar = try $ do 
  char '\\'
  (oneOf "abefnrtv\"'?\\" >>= return  . (\x -> ['\\',x]))
    <|> (do a <- oneOf "xX"
            b <- many1 hexDigit
            return ('\\':a:b))
    <|> (do a <- char '0'
            b <- many1 octDigit
            return ('\\':a:b))

pHlCChar :: GenParser Char st [Char]
pHlCChar = try $ do
  char '\''
  c <- pHlCStringChar
  char '\''
  return ('\'' : c ++ "'")

pRangeDetect :: Char -> Char -> GenParser Char st [Char]
pRangeDetect startChar endChar = try $ do
  char startChar
  body <- manyTill (noneOf ['\n', endChar]) (char endChar)
  return $ startChar : (body ++ [endChar])

pLineContinue :: GenParser Char st String
pLineContinue = try $ string "\\\n"

pDetectSpaces :: GenParser Char st [Char]
pDetectSpaces = many1 (oneOf "\t ")

pDetectIdentifier :: GenParser Char st [Char]
pDetectIdentifier = do
  first <- letter
  rest <- many alphaNum
  return (first:rest)

