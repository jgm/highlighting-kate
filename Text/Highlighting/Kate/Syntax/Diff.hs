{- This module was generated from data in the Kate syntax highlighting file diff.xml, version 1.10,
   by   -}

module Text.Highlighting.Kate.Syntax.Diff ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Diff"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.diff;*patch"

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
  setState $ st { synStLanguage = "Diff" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Diff",["Normal"])], synStLanguage = "Diff", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "FindDiff" -> return ()
    "File" -> return ()
    "Chunk" -> return ()
    "ChunkInFile" -> return ()
    "RFile" -> return ()
    "RChunk" -> return ()
    "RChunkInFile" -> return ()
    "RChunkNew" -> return ()
    "RChunkInFileNew" -> return ()
    "File" -> (popContext >> return ())
    "Removed" -> (popContext >> return ())
    "Added" -> (popContext >> return ())
    "ChangedOld" -> (popContext >> return ())
    "ChangedNew" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("File","Keyword"),("Header","DataType"),("Removed line","Others"),("Added line","String"),("Changed line (old)","Others"),("Changed line (new)","String")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("FindDiff","Normal Text"),("File","Normal Text"),("Chunk","Normal Text"),("ChunkInFile","Normal Text"),("RFile","Normal Text"),("RChunk","Normal Text"),("RChunkInFile","Normal Text"),("RChunkNew","Normal Text"),("RChunkInFileNew","Normal Text"),("File","File"),("Removed","Removed line"),("Added","Added line"),("ChangedOld","Changed line (old)"),("ChangedNew","Changed line (new)")]

parseRules "Normal" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(\\@\\@|\\d).*$") >>= withAttribute "Header") >>~ pushContext "Chunk")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\*+$") >>= withAttribute "Header") >>~ pushContext "RChunk")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "Only in .*:.*$") >>= withAttribute "File"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "diff.*$") >>= withAttribute "File") >>~ pushContext "RFile")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "(====|\\*\\*\\*|\\-\\-\\-).*$") >>= withAttribute "File") >>~ pushContext "File")
                        <|>
                        ((parseRules "FindDiff"))
                        <|>
                        ((pColumn 0 >> pDetectChar False '!' >>= withAttribute "Changed line") >>~ pushContext "ChangedOld"))
     return (attr, result)

parseRules "FindDiff" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(\\+\\+\\+|\\-\\-\\-).*$") >>= withAttribute "Header"))
                        <|>
                        ((pColumn 0 >> pAnyChar "+>" >>= withAttribute "Added line") >>~ pushContext "Added")
                        <|>
                        ((pColumn 0 >> pAnyChar "-<" >>= withAttribute "Removed line") >>~ pushContext "Removed"))
     return (attr, result)

parseRules "File" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(\\@\\@|\\d).*$") >>= withAttribute "Header") >>~ pushContext "ChunkInFile")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\*+$") >>= withAttribute "Header") >>~ pushContext "RChunkInFile")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "(====|\\*\\*\\*|\\-\\-\\-|diff|Only in .*:).*$") >>= withAttribute "File") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pDetectChar False '!' >>= withAttribute "Changed line (old)") >>~ pushContext "ChangedOld")
                        <|>
                        ((parseRules "FindDiff")))
     return (attr, result)

parseRules "Chunk" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(\\@\\@|\\d).*$") >>= withAttribute "Header") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pDetectChar False '!' >>= withAttribute "Changed line (old)") >>~ pushContext "ChangedOld")
                        <|>
                        ((parseRules "FindDiff")))
     return (attr, result)

parseRules "ChunkInFile" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(\\@\\@|\\d).*$") >>= withAttribute "Header") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "(====|\\*\\*\\*|\\-\\-\\-|diff|Only in .*:).*$") >>= withAttribute "File") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pDetectChar False '!' >>= withAttribute "Changed line (old)") >>~ pushContext "ChangedOld")
                        <|>
                        ((parseRules "FindDiff")))
     return (attr, result)

parseRules "RFile" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(diff|Only in .*:).*$") >>= withAttribute "File") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "(====|\\*\\*\\*|\\-\\-\\-|diff|Only in .*:).*$") >>= withAttribute "Header"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\*+$") >>= withAttribute "Header") >>~ pushContext "RChunkInFile")
                        <|>
                        ((parseRules "File")))
     return (attr, result)

parseRules "RChunk" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "\\*\\*\\* .* \\*\\*\\*\\*$") >>= withAttribute "Header"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\-\\-\\- .* \\-\\-\\-\\-$") >>= withAttribute "Header") >>~ pushContext "RChunkNew")
                        <|>
                        ((parseRules "Chunk")))
     return (attr, result)

parseRules "RChunkInFile" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "\\*\\*\\* .* \\*\\*\\*\\*$") >>= withAttribute "Header"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\-\\-\\- .* \\-\\-\\-\\-$") >>= withAttribute "Header") >>~ pushContext "RChunkInFileNew")
                        <|>
                        ((parseRules "ChunkInFile")))
     return (attr, result)

parseRules "RChunkNew" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(\\@\\@|\\d).*$") >>= withAttribute "Header") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pDetectChar False '!' >>= withAttribute "Changed line (new)") >>~ pushContext "ChangedNew")
                        <|>
                        ((parseRules "FindDiff")))
     return (attr, result)

parseRules "RChunkInFileNew" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(\\@\\@|\\d).*$") >>= withAttribute "Header") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "(====|\\*\\*\\*|\\-\\-\\-|diff|Only in .*:).*$") >>= withAttribute "File") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pDetectChar False '!' >>= withAttribute "Changed line (new)") >>~ pushContext "ChangedNew")
                        <|>
                        ((parseRules "FindDiff")))
     return (attr, result)

parseRules "File" = 
  pzero

parseRules "Removed" = 
  pzero

parseRules "Added" = 
  pzero

parseRules "ChangedOld" = 
  pzero

parseRules "ChangedNew" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x