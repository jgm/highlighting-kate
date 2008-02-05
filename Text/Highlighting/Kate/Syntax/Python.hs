{- This module was generated from data in the Kate syntax highlighting file python.xml, version 1.98,
   by  Michael Bueker -}

module Text.Highlighting.Kate.Syntax.Python ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Python"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.py;*.pyw"

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
  setState $ st { synStLanguage = "Python" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Python",["Normal"])], synStLanguage = "Python", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "parenthesised" -> return ()
    "Tripple A-comment" -> return ()
    "Tripple Q-comment" -> return ()
    "Tripple A-string" -> return ()
    "Raw Tripple A-string" -> return ()
    "Tripple Q-string" -> return ()
    "Raw Tripple Q-string" -> return ()
    "Single A-comment" -> return ()
    "Single Q-comment" -> return ()
    "Single A-string" -> return ()
    "Single Q-string" -> return ()
    "Raw A-string" -> return ()
    "Raw Q-string" -> return ()
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

styles = [("Normal Text","Normal"),("Definition Keyword","Keyword"),("Operator","Normal"),("String Substitution","Normal"),("Command Keyword","Keyword"),("Flow Control Keyword","Keyword"),("Builtin Function","DataType"),("Special Variable","Others"),("Extensions","Others"),("Preprocessor","Char"),("String Char","Char"),("Long","Others"),("Float","Float"),("Int","DecVal"),("Hex","Others"),("Octal","Others"),("Complex","Others"),("Comment","Comment"),("String","String"),("Raw String","String")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("parenthesised","Normal Text"),("Tripple A-comment","Comment"),("Tripple Q-comment","Comment"),("Tripple A-string","String"),("Raw Tripple A-string","Raw String"),("Tripple Q-string","String"),("Raw Tripple Q-string","Raw String"),("Single A-comment","Comment"),("Single Q-comment","Comment"),("Single A-string","String"),("Single Q-string","String"),("Raw A-string","Raw String"),("Raw Q-string","Raw String")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword ["import","from","as"] >>= withAttribute "Preprocessor"))
                        <|>
                        ((pKeyword ["class","def","del","global","lambda"] >>= withAttribute "Definition Keyword"))
                        <|>
                        ((pKeyword ["and","assert","in","is","not","or"] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["exec","print"] >>= withAttribute "Command Keyword"))
                        <|>
                        ((pKeyword ["break","continue","elif","else","except","finally","for","if","pass","raise","return","try","while","yield"] >>= withAttribute "Flow Control Keyword"))
                        <|>
                        ((pKeyword ["__import__","abs","all","any","apply","basestring","bool","buffer","callable","chr","classmethod","cmp","coerce","compile","complex","delattr","dict","dir","divmod","enumerate","eval","execfile","file","filter","float","frozenset","getattr","globals","hasattr","hash","hex","id","input","int","intern","isinstance","issubclass","iter","len","list","locals","long","map","max","min","object","oct","open","ord","pow","property","range","raw_input","reduce","reload","repr","reversed","round","set","setattr","slice","sorted","staticmethod","str","sum","super","tuple","type","unichr","unicode","vars","xrange","zip"] >>= withAttribute "Builtin Function"))
                        <|>
                        ((pKeyword ["None","self","True","False","NotImplemented","Ellipsis"] >>= withAttribute "Special Variable"))
                        <|>
                        ((pKeyword ["SIGNAL","SLOT","connect"] >>= withAttribute "Extensions"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z_][a-zA-Z_0-9]+") >>= withAttribute "Normal"))
                        <|>
                        ((pRegExpr (compileRegex " ((([0-9]*\\.[0-9]+|[0-9]+\\.)|([0-9]+|([0-9]*\\.[0-9]+|[0-9]+\\.))[eE](\\+|-)?[0-9]+)|[0-9]+)[jJ]") >>= withAttribute "Complex"))
                        <|>
                        ((pRegExpr (compileRegex "([0-9]+\\.[0-9]*|\\.[0-9]+)([eE][0-9]+)?") >>= withAttribute "Float"))
                        <|>
                        ((pRegExpr (compileRegex "([1-9][0-9]*([eE][0-9]+)?|0)") >>= withAttribute "Int"))
                        <|>
                        ((pRegExpr (compileRegex "[1-9][0-9]*([eE][0-9.]+)?[Ll]") >>= withAttribute "Long"))
                        <|>
                        ((pRegExpr (compileRegex "0[Xx][0-9a-fA-F]+") >>= withAttribute "Hex"))
                        <|>
                        ((pRegExpr (compileRegex "0[1-9][0-9]*") >>= withAttribute "Octal"))
                        <|>
                        ((pRegExpr (compileRegex "[rR]'''") >>= withAttribute "Raw String") >>~ pushContext "Raw Tripple A-string")
                        <|>
                        ((pRegExpr (compileRegex "[rR]\"\"\"") >>= withAttribute "Raw String") >>~ pushContext "Raw Tripple Q-string")
                        <|>
                        ((pRegExpr (compileRegex "[rR]'") >>= withAttribute "Raw String") >>~ pushContext "Raw A-string")
                        <|>
                        ((pRegExpr (compileRegex "[rR]\"") >>= withAttribute "Raw String") >>~ pushContext "Raw Q-string")
                        <|>
                        ((pRegExpr (compileRegex "#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\s*'''") >>= withAttribute "Comment") >>~ pushContext "Tripple A-comment")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\s*\"\"\"") >>= withAttribute "Comment") >>~ pushContext "Tripple Q-comment")
                        <|>
                        ((pString False "'''" >>= withAttribute "String") >>~ pushContext "Tripple A-string")
                        <|>
                        ((pString False "\"\"\"" >>= withAttribute "String") >>~ pushContext "Tripple Q-string")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "Single A-string")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "Single Q-string")
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Operator") >>~ pushContext "parenthesised")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Operator") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[+*/%\\|=;\\!<>!^&~-]") >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution")))
     return (attr, result)

parseRules "parenthesised" = 
  do (attr, result) <- ((parseRules "Normal"))
     return (attr, result)

parseRules "Tripple A-comment" = 
  do (attr, result) <- ((pString False "'''" >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Tripple Q-comment" = 
  do (attr, result) <- (((pHlCChar >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\"\"\"") >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Tripple A-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "'''") >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Raw Tripple A-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "Raw String"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "'''") >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Tripple Q-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "\"\"\"") >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Raw Tripple Q-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "Raw String"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "\"\"\"") >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Single A-comment" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "Comment"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Single Q-comment" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "Comment"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Single A-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Single Q-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Raw A-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "Raw String"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Raw String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Raw Q-string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "Raw String"))
                        <|>
                        ((pRegExpr (compileRegex "%\\([a-zA-Z0-9_]+\\)[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "%[a-zA-Z]") >>= withAttribute "String Substitution"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Raw String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x