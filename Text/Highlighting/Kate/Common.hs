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
import Data.ByteString.UTF8 (fromString, toString)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.ByteString
#endif
import Text.Highlighting.Kate.Types
import Text.ParserCombinators.Parsec hiding (State)
import Data.Char (isDigit, toLower, isSpace)
import Data.List (tails)
import Control.Monad.State
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

normalizeHighlighting :: [Token] -> [Token]
normalizeHighlighting [] = []
normalizeHighlighting ((_,""):xs) = normalizeHighlighting xs
normalizeHighlighting ((NormalTok,x):xs)
  | all isSpace x = (NormalTok,x) : normalizeHighlighting xs
normalizeHighlighting ((a,x):(b,y):xs)
  | a == b = normalizeHighlighting ((a, x++y):xs)
normalizeHighlighting (x:xs) = x : normalizeHighlighting xs

pushContext :: Context -> KateParser ()
pushContext (lang,context) =
                      if context == "#stay"
                         then return ()
                         else do st <- getState
                                 let contexts = synStContexts st
                                 updateState $ \st -> st{ synStContexts =
                                                (lang,context) : contexts }

popContext :: KateParser ()
popContext = do st <- getState
                case synStContexts st of
                    [_]    -> return () -- stay if we're at the root
                    (_:xs) -> updateState $ \st -> st{ synStContexts = xs }
                    []     -> fail "Stack empty"

currentContext :: KateParser Context
currentContext = do st <- getState
                    case synStContexts st of
                         (x:_) -> return x
                         []    -> fail "Stack empty"

withChildren :: KateParser Token
             -> KateParser Token
             -> KateParser Token
withChildren parent child = do
  (pAttr, pResult) <- parent
  (_, cResult) <- option (NormalTok,"") child
  return (pAttr, pResult ++ cResult)

pFirstNonSpace :: KateParser ()
pFirstNonSpace = do
  rest <- getInput
  prevNonspace <- fromState synStPrevNonspace
  guard $ not $ prevNonspace || null rest || isSpace (head rest)

currentColumn :: GenParser tok st Column
currentColumn = sourceColumn `fmap` getPosition

pColumn :: Column -> GenParser tok st ()
pColumn col = do
  curCol <- currentColumn
  guard $ col == (curCol - 1) -- parsec's columns start with 1

pGetCapture :: Int -> KateParser String
pGetCapture capNum = do
  captures <- getState >>= return . synStCaptures
  if length captures < capNum
     then fail "Not enough captures"
     else return $ captures !! (capNum - 1)

pDetectChar :: Bool -> Char -> KateParser String
pDetectChar dynamic ch = do
  if dynamic && isDigit ch
     then pGetCapture (read [ch]) >>= try . string
     else char ch >>= return . (:[])

pDetect2Chars :: Bool -> Char -> Char -> KateParser [Char]
pDetect2Chars dynamic ch1 ch2 = try $ do
  [c1] <- pDetectChar dynamic ch1
  [c2] <- pDetectChar dynamic ch2
  return [c1, c2]

pKeyword :: [Char] -> Set.Set [Char] -> KateParser [Char]
pKeyword delims kws = try $ do
  notFollowedBy (oneOf delims)
  prevChar <- fromState synStPrevChar
  caseSensitive <- fromState synStKeywordCaseSensitive
  guard $ prevChar `elem` delims
  word <- many1 (noneOf delims)
  let word' = if caseSensitive
                 then word
                 else map toLower word
  if word' `Set.member` kws
     then return word
     else fail "Keyword not in list"

pString :: Bool -> [Char] -> KateParser String
pString dynamic str =
  if dynamic
     then subDynamic str >>= try . string
     else try $ string str

pAnyChar :: [Char] -> KateParser [Char]
pAnyChar chars = oneOf chars >>= return . (:[])

pDefault :: KateParser [Char]
pDefault = (:[]) `fmap` anyChar

subDynamic :: [Char] -> KateParser [Char]
subDynamic ('%':x:xs) | isDigit x = do
  captures <- getState >>= return . synStCaptures
  let capNum = read [x]
  let escapeRegexChar c | c `elem` "^$\\[](){}*+.?" = ['\\',c]
                        | otherwise = [c]
  let escapeRegex = concatMap escapeRegexChar
  let replacement = if length captures < capNum
                       then ['%',x]
                       else captures !! (capNum - 1)
  subDynamic xs >>= return . (escapeRegex replacement ++)
subDynamic (x:xs) = subDynamic xs >>= return . (x:)
subDynamic "" = return ""

convertOctal :: String -> String
convertOctal [] = ""
convertOctal ('\\':'0':x:y:z:rest) | isOctalDigit x && isOctalDigit y && isOctalDigit z =
  case reads ['\\','o',x,y,z] of
        ((x,rest):_) -> x : convertOctal rest
        _            -> '\\':'o':x:y:z: convertOctal rest
convertOctal ('\\':x:y:z:rest) | isOctalDigit x && isOctalDigit y && isOctalDigit z =
  case reads ['\\','o',x,y,z] of
        ((x,rest):_) -> x : convertOctal rest
        _            -> '\\':'o':x:y:z: convertOctal rest
convertOctal (x:xs) = x : convertOctal xs

isOctalDigit :: Char -> Bool
isOctalDigit c = c == '0' || c == '1' || c == '2' || c == '3'
              || c == '4' || c == '5' || c == '6' || c == '7'

compileRegex :: Bool -> String -> Regex
compileRegex caseSensitive regexpStr =
#ifdef _PCRE_LIGHT
  let opts = [anchored] ++ [caseless | not caseSensitive]
  in  compile ('.' : convertOctal regexpStr) opts
#else
  let opts = compAnchored + compUTF8 +
               if caseSensitive then 0 else compCaseless
  in  case unsafePerformIO $ compile opts (execNotEmpty)
           (fromString ('.' : convertOctal regexpStr)) of
            Left _  -> error $ "Error compiling regex: " ++ show regexpStr
            Right r -> r
#endif

matchRegex :: Regex -> String -> KateParser (Maybe [String])
#ifdef _PCRE_LIGHT
matchRegex r s = return $ match r s [exec_notempty]
#else
matchRegex r s = case unsafePerformIO (regexec r (fromString s)) of
                      Right (Just (_, mat, _ , capts)) -> return $
                                       Just $ map toString (mat : capts)
                      Right Nothing -> return Nothing
                      Left matchError -> fail $ show matchError
#endif

pRegExpr :: Regex -> KateParser String
pRegExpr regex = do
  rest <- getInput
  prevChar <- fromState synStPrevChar
  -- Note: we keep one preceding character, so initial \b can match or not...
  let target = if prevChar == '\n'
                  then ' ':rest
                  else prevChar:rest
  matches <- matchRegex regex target
  case matches of
        Just (x:xs) | null x -> fail "Regex matched null string!"
                    | otherwise -> do
                          unless (null xs) $
                            updateState (\st -> st {synStCaptures = xs})
                          count (length x - 1) anyChar
        _           -> pzero

pRegExprDynamic :: [Char] -> KateParser String
pRegExprDynamic regexpStr = do
  regexpStr' <- subDynamic regexpStr
  caseSensitive <- synStCaseSensitive `fmap` getState
  pRegExpr $ compileRegex caseSensitive regexpStr'

integerRegex :: Regex
integerRegex =
  compileRegex True "\\b[-+]?(0[Xx][0-9A-Fa-f]+|0[Oo][0-7]+|[0-9]+)\\b"

pInt :: KateParser String
pInt = pRegExpr integerRegex

floatRegex :: Regex
floatRegex = compileRegex True "\\b[-+]?(([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+)\\b"

pFloat :: KateParser String
pFloat = pRegExpr floatRegex

octRegex :: Regex
octRegex = compileRegex True "\\b[-+]?0[Oo][0-7]+\\b"

pHlCOct :: KateParser String
pHlCOct = pRegExpr octRegex

hexRegex :: Regex
hexRegex = compileRegex True "\\b[-+]?0[Xx][0-9A-Fa-f]+\\b"

pHlCHex :: KateParser String
pHlCHex = pRegExpr hexRegex

pHlCStringChar :: KateParser [Char]
pHlCStringChar = try $ do
  char '\\'
  (oneOf "abefnrtv\"'?\\" >>= return  . (\x -> ['\\',x]))
    <|> (do a <- satisfy (\c -> c == 'x' || c == 'X')
            b <- many1 hexDigit
            return ('\\':a:b))
    <|> (do a <- char '0'
            b <- many1 octDigit
            return ('\\':a:b))

pHlCChar :: KateParser [Char]
pHlCChar = try $ do
  char '\''
  c <- pHlCStringChar
  char '\''
  return ('\'' : c ++ "'")

pRangeDetect :: Char -> Char -> KateParser [Char]
pRangeDetect startChar endChar = try $ do
  char startChar
  body <- manyTill (satisfy (/= endChar)) (char endChar)
  return $ startChar : (body ++ [endChar])

pLineContinue :: KateParser String
pLineContinue = try $ char '\\' >> eof >> return "\\"

pDetectSpaces :: KateParser [Char]
pDetectSpaces = many1 (satisfy $ \c -> c == ' ' || c == '\t')

pDetectIdentifier :: KateParser [Char]
pDetectIdentifier = do
  first <- letter
  rest <- many alphaNum
  return (first:rest)

fromState :: (SyntaxState -> a) -> KateParser a
fromState f = f `fmap` getState

mkParseSourceLine :: KateParser Token    -- ^ parseExpressionInternal
                  -> String
                  -> State SyntaxState SourceLine
mkParseSourceLine parseExpression ln = do
  modify $ \st -> st{ synStLineNumber = synStLineNumber st + 1 }
  st <- get
  let lineName = "line " ++ show (synStLineNumber st)
  let pline = do ts <- manyTill parseExpression eof
                 s  <- getState
                 return (s, ts)
  let (newst, result) = case runParser pline st lineName ln of
                              Left _      -> (st, [(ErrorTok,ln)])
                              Right (s,r) -> (s,r)
  put $! newst
  return $! normalizeHighlighting result
