{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
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
import Text.ParserCombinators.Parsec hiding (State)
import Data.Char (isDigit, toLower, isSpace)
import Data.List (tails)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Match filename against a list of globs contained in a semicolon-separated
-- string.
matchGlobs :: String -> String -> Bool
matchGlobs fn globs = either (const False) (any (flip matchGlob fn)) (splitBySemi $ filter (/=' ') globs)

-- | Match filename against a glob pattern with asterisks.
matchGlob :: String -> String -> Bool
matchGlob ('*':xs) fn = any (matchGlob xs) (tails fn)
matchGlob (x:xs) (y:ys) = x == y && matchGlob xs ys
matchGlob "" "" = True
matchGlob _ _   = False

-- | Splits semicolon-separated list
splitBySemi :: String -> Either String [String]
splitBySemi "" = Right []
splitBySemi xs =
  let (pref, suff) = break (==';') xs
  in  case suff of
         []       -> Right [pref]
         (';':ys) -> fmap (pref :) (splitBySemi ys)
         _        -> Left "The impossible happened (splitBySemi)"

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

pushContext :: [Char] -> KateParser ()
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

popContext :: KateParser ()
popContext = do st <- getState
                let contexts = synStContexts st
                let lang = synStLanguage st
                case Map.lookup lang contexts of
                    Just (_:_) -> updateState $ \st ->
                                    st{ synStContexts = Map.adjust tail lang contexts }
                    Just []    -> fail $ "Stack empty for language " ++ lang
                    Nothing    -> fail $ "No context stack for language " ++ lang

currentContext :: KateParser String
currentContext = do st <- getState
                    let contexts = synStContexts st
                    let lang = synStLanguage st
                    case Map.lookup lang contexts of
                         Just []    -> return ""
                         Just (c:_) -> return c
                         Nothing    -> fail $ "No context stack for language " ++ lang

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

compileRegex :: String -> Either String Regex
#ifdef _PCRE_LIGHT
compileRegex regexpStr = compile ('.' : convertOctal regexpStr) [anchored]
#else
compileRegex regexpStr =
  case unsafePerformIO $ compile (compAnchored) (execNotEmpty)
       ('.' : convertOctal regexpStr) of
        Left _ -> Left $ "Error compiling regex: " ++ show regexpStr
        Right r -> Right r
#endif

matchRegex :: Regex -> String -> Either String (Maybe [String])
#ifdef _PCRE_LIGHT
matchRegex r s = match r s [exec_notempty]
#else
matchRegex r s = case unsafePerformIO (regexec r s) of
                      Right (Just (_, mat, _ , capts)) -> Right $ Just (mat : capts)
                      Right Nothing -> Right Nothing
                      Left matchError -> Left $ show matchError
#endif

pRegExpr :: Either String Regex -> KateParser String
pRegExpr (Left err) = fail err
pRegExpr (Right compiledRegex) = do
  rest <- getInput
  prevChar <- fromState synStPrevChar
  -- Note: we keep one preceding character, so initial \b can match or not...
  let target = if prevChar == '\n'
                  then ' ':rest
                  else prevChar:rest
  case matchRegex compiledRegex target of
        Right (Just (x:xs))
          | null x -> fail "Regex matched null string!"
          | otherwise -> do
            unless (null xs) $
              updateState (\st -> st {synStCaptures = xs})
            count (length x - 1) anyChar
        _           -> pzero

pRegExprDynamic :: [Char] -> KateParser String
pRegExprDynamic regexpStr = do
  regexpStr' <- subDynamic regexpStr
  let compiledRegex = compileRegex regexpStr'
  pRegExpr compiledRegex

integerRegex :: Either String Regex
integerRegex = compileRegex "\\b[-+]?(0[Xx][0-9A-Fa-f]+|0[Oo][0-7]+|[0-9]+)\\b"

pInt :: KateParser String
pInt = pRegExpr integerRegex

floatRegex :: Either String Regex
floatRegex = compileRegex "\\b[-+]?(([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([Ee][-+]?[0-9]+)?|[0-9]+[Ee][-+]?[0-9]+)\\b"

pFloat :: KateParser String
pFloat = pRegExpr floatRegex

octRegex :: Either String Regex
octRegex = compileRegex "\\b[-+]?0[Oo][0-7]+\\b"

pHlCOct :: KateParser String
pHlCOct = pRegExpr octRegex

hexRegex :: Either String Regex
hexRegex = compileRegex "\\b[-+]?0[Xx][0-9A-Fa-f]+\\b"

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
                  -> KateParser ()       -- ^ pEndline
                  -> String
                  -> State SyntaxState SourceLine
mkParseSourceLine parseExpressionInternal pEndLine ln = do
  modify $ \st -> st{ synStLineNumber = synStLineNumber st + 1 }
  st <- get
  let lineName = "line " ++ show (synStLineNumber st)
  let pline = do ts <- many parseExpressionInternal
                 updateState $ \st -> st{ synStPrevChar = '\n' }
                 pEndLine
                 s  <- getState
                 return (s, ts)
  let (newst, result) = case runParser pline st lineName ln of
                              Left _      -> (st, [(ErrorTok,ln)])
                              Right (s,r) -> (s,r)
  put $! newst
  return $! normalizeHighlighting result
