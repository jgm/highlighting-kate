{- This module was generated from data in the Kate syntax highlighting file latex.xml, version 1.26,
   by  Jeroen Wijnhout (Jeroen.Wijnhout@kdemail.net)+Holger Danielsson (holger.danielsson@versanet.de)+Michel Ludwig (michel.ludwig@kdemail.net) -}

module Text.Highlighting.Kate.Syntax.Latex ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "LaTeX"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.tex; *.ltx; *.dtx; *.sty; *.cls;"

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
  setState $ st { synStLanguage = "LaTeX" }
  context <- currentContext <|> (pushContext "Normal Text" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("LaTeX",["Normal Text"])], synStLanguage = "LaTeX", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal Text" -> return ()
    "Sectioning" -> return ()
    "SectioningInside" -> return ()
    "SectioningContrSeq" -> (popContext >> return ())
    "SectioningMathMode" -> return ()
    "SectioningMathContrSeq" -> (popContext >> return ())
    "NewCommand" -> return ()
    "DefCommand" -> return ()
    "CommandParameterStart" -> return ()
    "CommandParameter" -> return ()
    "ContrSeq" -> (popContext >> return ())
    "ToEndOfLine" -> (popContext >> return ())
    "Verb" -> (popContext >> popContext >> return ())
    "VerbEnd" -> (popContext >> popContext >> popContext >> return ())
    "Label" -> return ()
    "LabelOption" -> return ()
    "LabelParameter" -> return ()
    "FindEnvironment" -> return ()
    "Environment" -> return ()
    "LatexEnv" -> return ()
    "VerbatimEnv" -> return ()
    "VerbatimEnvParam" -> return ()
    "Verbatim" -> return ()
    "VerbFindEnd" -> (popContext >> return ())
    "MathEnv" -> return ()
    "MathEnvParam" -> return ()
    "EnvCommon" -> return ()
    "MathModeEnv" -> return ()
    "MathFindEnd" -> (popContext >> return ())
    "MathMode" -> return ()
    "MathModeDisplay" -> return ()
    "MathModeEquation" -> return ()
    "MathModeCommon" -> return ()
    "MathContrSeq" -> (popContext >> return ())
    "MathModeText" -> return ()
    "MathModeTextParameterStart" -> return ()
    "MathModeTextParameter" -> return ()
    "Comment" -> (popContext >> return ())
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }

withAttribute attr txt = do
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Keyword","Normal"),("Comment","Comment"),("Error","Alert"),("Math","Normal"),("Structure","Normal"),("Keyword Mathmode","Normal"),("Environment","Normal"),("Verbatim","Normal"),("Region Marker","RegionMarker"),("Bullet","Normal"),("Alert","Alert"),("Structure Text","Normal"),("Structure Keyword","Normal"),("Structure Math","Normal"),("Structure Keyword Mathmode","Normal")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("Sectioning","Normal Text"),("SectioningInside","Structure Text"),("SectioningContrSeq","Keyword"),("SectioningMathMode","Structure Math"),("SectioningMathContrSeq","Structure Keyword Mathmode"),("NewCommand","Normal Text"),("DefCommand","Normal Text"),("CommandParameterStart","Normal Text"),("CommandParameter","Normal Text"),("ContrSeq","Keyword"),("ToEndOfLine","Normal Text"),("Verb","Verbatim"),("VerbEnd","Verbatim"),("Label","Normal Text"),("LabelOption","Normal Text"),("LabelParameter","Environment"),("FindEnvironment","Normal Text"),("Environment","Environment"),("LatexEnv","Environment"),("VerbatimEnv","Environment"),("VerbatimEnvParam","Normal Text"),("Verbatim","Verbatim"),("VerbFindEnd","Normal Text"),("MathEnv","Environment"),("MathEnvParam","Normal Text"),("EnvCommon","Environment"),("MathModeEnv","Math"),("MathFindEnd","Normal Text"),("MathMode","Math"),("MathModeDisplay","Math"),("MathModeEquation","Math"),("MathModeCommon","Math"),("MathContrSeq","Keyword Mathmode"),("MathModeText","Normal Text"),("MathModeTextParameterStart","Normal Text"),("MathModeTextParameter","Normal Text"),("Comment","Comment")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\begin(?=[^a-zA-Z])") >>= withAttribute "Structure") >>~ pushContext "FindEnvironment")
                        <|>
                        ((pRegExpr (compileRegex "\\\\end(?=[^a-zA-Z])") >>= withAttribute "Structure") >>~ pushContext "FindEnvironment")
                        <|>
                        ((pRegExpr (compileRegex "\\\\(label|pageref|ref|vpageref|vref|cite)(?=[^a-zA-Z])") >>= withAttribute "Structure") >>~ pushContext "Label")
                        <|>
                        ((pRegExpr (compileRegex "\\\\(part|chapter|section|subsection|subsubsection|paragraph|subparagraph)\\*?\\s*(?=[\\{\\[])") >>= withAttribute "Structure") >>~ pushContext "Sectioning")
                        <|>
                        ((pRegExpr (compileRegex "\\\\(re)?newcommand(?=[^a-zA-Z])") >>= withAttribute "Keyword") >>~ pushContext "NewCommand")
                        <|>
                        ((pRegExpr (compileRegex "\\\\(e|g|x)?def(?=[^a-zA-Z])") >>= withAttribute "Keyword") >>~ pushContext "DefCommand")
                        <|>
                        ((pString False "\\(" >>= withAttribute "Math") >>~ pushContext "MathMode")
                        <|>
                        ((pString False "\\[" >>= withAttribute "Math") >>~ pushContext "MathModeEquation")
                        <|>
                        ((pDetectChar False '\\' >>= withAttribute "Keyword") >>~ pushContext "ContrSeq")
                        <|>
                        ((pString False "$$" >>= withAttribute "Math") >>~ pushContext "MathModeDisplay")
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Math") >>~ pushContext "MathMode")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "%\\s*BEGIN.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "%\\s*END.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet")))
     return (attr, result)

parseRules "Sectioning" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\[[^\\]]*\\]") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False ' ' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "SectioningInside")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "SectioningInside" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "SectioningInside")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pString False "\\(" >>= withAttribute "Structure Math") >>~ pushContext "SectioningMathMode")
                        <|>
                        ((pDetectChar False '\\' >>= withAttribute "Structure Keyword") >>~ pushContext "SectioningContrSeq")
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Structure Math") >>~ pushContext "SectioningMathMode")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet")))
     return (attr, result)

parseRules "SectioningContrSeq" = 
  do (attr, result) <- (((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]+(\\+?|\\*{0,3})") >>= withAttribute "Structure Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^a-zA-Z]") >>= withAttribute "Structure Keyword") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "SectioningMathMode" = 
  do (attr, result) <- (((pString False "$$" >>= withAttribute "Error"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Structure Math") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Structure Math") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Error"))
                        <|>
                        ((pDetectChar False '\\' >>= withAttribute "Structure Keyword Mathmode") >>~ pushContext "SectioningMathContrSeq")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet")))
     return (attr, result)

parseRules "SectioningMathContrSeq" = 
  do (attr, result) <- (((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]+\\*?") >>= withAttribute "Structure Keyword Mathmode") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^a-zA-Z]") >>= withAttribute "Structure Keyword Mathmode") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "NewCommand" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*\\{\\s*\\\\[a-zA-Z]+\\s*\\}(\\[\\d\\](\\[[^\\]]+\\])?)?\\{") >>= withAttribute "Normal Text") >>~ pushContext "CommandParameterStart")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Error") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "DefCommand" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*\\\\[a-zA-Z]+[^\\{]*\\{") >>= withAttribute "Normal Text") >>~ pushContext "CommandParameterStart")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Error") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "CommandParameterStart" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "CommandParameter")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment"))
     return (attr, result)

parseRules "CommandParameter" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "CommandParameter")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment"))
     return (attr, result)

parseRules "ContrSeq" = 
  do (attr, result) <- (((pString False "verb*" >>= withAttribute "Keyword") >>~ pushContext "Verb")
                        <|>
                        ((pRegExpr (compileRegex "verb(?=[^a-zA-Z])") >>= withAttribute "Keyword") >>~ pushContext "Verb")
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]+(\\+?|\\*{0,3})") >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^a-zA-Z]") >>= withAttribute "Keyword") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "ToEndOfLine" = 
  pzero

parseRules "Verb" = 
  do (attr, result) <- ((pRegExprDynamic "(.)" >>= withAttribute "Normal Text") >>~ pushContext "VerbEnd")
     return (attr, result)

parseRules "VerbEnd" = 
  do (attr, result) <- (((pString True "%1" >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExprDynamic "[^%1\\xd7]*" >>= withAttribute "Verbatim")))
     return (attr, result)

parseRules "Label" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*\\{\\s*") >>= withAttribute "Normal Text") >>~ pushContext "LabelParameter")
                        <|>
                        ((pRegExpr (compileRegex "\\s*\\[\\s*") >>= withAttribute "Normal Text") >>~ pushContext "LabelOption")
                        <|>
                        ((pRegExpr (compileRegex "[^\\[\\{]+") >>= withAttribute "Error")))
     return (attr, result)

parseRules "LabelOption" = 
  do (attr, result) <- (((pString False "\\(" >>= withAttribute "Math") >>~ pushContext "MathMode")
                        <|>
                        ((pDetectChar False '\\' >>= withAttribute "Keyword") >>~ pushContext "ContrSeq")
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Math") >>~ pushContext "MathMode")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "\\s*\\]\\s*") >>= withAttribute "Normal Text") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "LabelParameter" = 
  do (attr, result) <- (((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "\\s*\\}\\s*") >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "FindEnvironment" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "Environment")
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Normal Text") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Environment" = 
  do (attr, result) <- (((pRegExpr (compileRegex "(lstlisting|(B|L)?Verbatim)") >>= withAttribute "Environment") >>~ pushContext "VerbatimEnvParam")
                        <|>
                        ((pRegExpr (compileRegex "(verbatim|boxedverbatim)") >>= withAttribute "Environment") >>~ pushContext "VerbatimEnv")
                        <|>
                        ((pRegExpr (compileRegex "(equation|displaymath|eqnarray|subeqnarray|math|multline|gather|align|flalign)") >>= withAttribute "Environment") >>~ pushContext "MathEnv")
                        <|>
                        ((pRegExpr (compileRegex "(alignat|xalignat|xxalignat)") >>= withAttribute "Environment") >>~ pushContext "MathEnvParam")
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]") >>= withAttribute "Environment") >>~ pushContext "LatexEnv")
                        <|>
                        ((pRegExpr (compileRegex "\\s+") >>= withAttribute "Error") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^a-zA-Z\\xd7]") >>= withAttribute "Error") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "LatexEnv" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]+") >>= withAttribute "Environment"))
                        <|>
                        ((pRegExpr (compileRegex "\\s+") >>= withAttribute "Error"))
                        <|>
                        ((parseRules "EnvCommon")))
     return (attr, result)

parseRules "VerbatimEnv" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ pushContext "Verbatim")
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]") >>= withAttribute "Environment") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "EnvCommon"))
                        <|>
                        ((popContext >> popContext >> popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "VerbatimEnvParam" = 
  do (attr, result) <- (((pDetect2Chars False '}' '[' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ pushContext "Verbatim")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Normal Text") >>~ pushContext "Verbatim"))
     return (attr, result)

parseRules "Verbatim" = 
  do (attr, result) <- (((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\end(?=\\s*\\{(verbatim|lstlisting|boxedverbatim|(B|L)?Verbatim)\\*?\\})") >>= withAttribute "Structure") >>~ pushContext "VerbFindEnd"))
     return (attr, result)

parseRules "VerbFindEnd" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*\\{") >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "(verbatim|lstlisting|boxedverbatim|(B|L)?Verbatim)\\*?") >>= withAttribute "Environment"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "MathEnv" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ pushContext "MathModeEnv")
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]") >>= withAttribute "Environment") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "EnvCommon")))
     return (attr, result)

parseRules "MathEnvParam" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\}\\{[^\\}]*\\}") >>= withAttribute "Normal Text") >>~ pushContext "MathModeEnv")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ pushContext "MathModeEnv")
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]") >>= withAttribute "Environment") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "EnvCommon")))
     return (attr, result)

parseRules "EnvCommon" = 
  do (attr, result) <- (((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "\\*(?=\\})") >>= withAttribute "Environment"))
                        <|>
                        ((pRegExpr (compileRegex "\\*[^\\}]*") >>= withAttribute "Error") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^a-zA-Z\\xd7][^\\}]*") >>= withAttribute "Error") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "MathModeEnv" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\end(?=\\s*\\{(equation|displaymath|eqnarray|subeqnarray|math|multline|gather|align|flalign|alignat|xalignat|xxalignat)\\*?\\})") >>= withAttribute "Structure") >>~ pushContext "MathFindEnd")
                        <|>
                        ((pRegExpr (compileRegex "\\\\begin(?=[^a-zA-Z])") >>= withAttribute "Keyword Mathmode"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\end(?=[^a-zA-Z])") >>= withAttribute "Keyword Mathmode"))
                        <|>
                        ((pString False "\\(" >>= withAttribute "Error"))
                        <|>
                        ((pString False "\\[" >>= withAttribute "Error"))
                        <|>
                        ((pString False "\\)" >>= withAttribute "Error"))
                        <|>
                        ((pString False "\\]" >>= withAttribute "Error"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\(text|intertext|mbox)\\s*(?=\\{)") >>= withAttribute "Keyword Mathmode") >>~ pushContext "MathModeText")
                        <|>
                        ((pDetectChar False '\\' >>= withAttribute "Keyword Mathmode") >>~ pushContext "MathContrSeq")
                        <|>
                        ((pString False "$$" >>= withAttribute "Error"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Error"))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "%\\s*BEGIN.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "%\\s*END.*$") >>= withAttribute "Region Marker")))
     return (attr, result)

parseRules "MathFindEnd" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*\\{") >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "(equation|displaymath|eqnarray|subeqnarray|math|multline|gather|align|flalign|alignat|xalignat|xxalignat)\\*?") >>= withAttribute "Environment"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "MathMode" = 
  do (attr, result) <- (((pString False "$$" >>= withAttribute "Error"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Math") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Math") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Error"))
                        <|>
                        ((parseRules "MathModeCommon")))
     return (attr, result)

parseRules "MathModeDisplay" = 
  do (attr, result) <- (((pString False "$$" >>= withAttribute "Math") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Error"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Error"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Error"))
                        <|>
                        ((parseRules "MathModeCommon")))
     return (attr, result)

parseRules "MathModeEquation" = 
  do (attr, result) <- (((pDetect2Chars False '\\' ']' >>= withAttribute "Math") >>~ (popContext >> return ()))
                        <|>
                        ((pString False "$$" >>= withAttribute "Error"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Error"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Error"))
                        <|>
                        ((parseRules "MathModeCommon")))
     return (attr, result)

parseRules "MathModeCommon" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\(begin|end)\\s*\\{(equation|displaymath|eqnarray|subeqnarray|math|multline|gather|align|flalign|alignat|xalignat|xxalignat)\\*?\\}") >>= withAttribute "Error"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\begin(?=[^a-zA-Z])") >>= withAttribute "Keyword Mathmode"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\end(?=[^a-zA-Z])") >>= withAttribute "Keyword Mathmode"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\(text|intertext|mbox)\\s*(?=\\{)") >>= withAttribute "Keyword Mathmode") >>~ pushContext "MathModeText")
                        <|>
                        ((pDetectChar False '\\' >>= withAttribute "Keyword Mathmode") >>~ pushContext "MathContrSeq")
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "%\\s*BEGIN.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "%\\s*END.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet")))
     return (attr, result)

parseRules "MathContrSeq" = 
  do (attr, result) <- (((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z]+\\*?") >>= withAttribute "Keyword Mathmode") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[^a-zA-Z]") >>= withAttribute "Keyword Mathmode") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "MathModeText" = 
  do (attr, result) <- ((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "MathModeTextParameterStart")
     return (attr, result)

parseRules "MathModeTextParameterStart" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "MathModeTextParameter")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment"))
     return (attr, result)

parseRules "MathModeTextParameter" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "MathModeTextParameter")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet"))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "Comment"))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pRegExpr (compileRegex "(FIXME|TODO):?") >>= withAttribute "Alert"))
                        <|>
                        ((pDetectChar False '\215' >>= withAttribute "Bullet")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x