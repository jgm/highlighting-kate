{- This module was generated from data in the Kate syntax highlighting file asp.xml, version 1.03,
   by  Antonio Salazar (savedfastcool@gmail.com) -}

module Text.Highlighting.Kate.Syntax.Asp ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "ASP"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.asp;"

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
  setState $ st { synStLanguage = "ASP" }
  context <- currentContext <|> (pushContext "nosource" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("ASP",["nosource"])], synStLanguage = "ASP", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "nosource" -> return ()
    "aspsource" -> return ()
    "asp_onelinecomment" -> (popContext >> return ())
    "doublequotestring" -> return ()
    "singlequotestring" -> return ()
    "htmltag" -> return ()
    "htmlcomment" -> return ()
    "identifiers" -> return ()
    "types1" -> return ()
    "types2" -> return ()
    "scripts" -> return ()
    "scripts_onelinecomment" -> (popContext >> return ())
    "twolinecomment" -> return ()
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

styles = [("Normal Text","Normal"),("ASP Text","Normal"),("Keyword","Keyword"),("Function","Keyword"),("Decimal","DecVal"),("Octal","BaseN"),("Hex","BaseN"),("Float","Float"),("String","String"),("Comment","Comment"),("Variable","Keyword"),("Control Structures","Keyword"),("Escape Code","Keyword"),("Other","Others"),("HTML Tag","Keyword"),("HTML Comment","Comment"),("Identifier","Others"),("Types","DataType")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("nosource","Normal Text"),("aspsource","ASP Text"),("asp_onelinecomment","Comment"),("doublequotestring","String"),("singlequotestring","String"),("htmltag","Identifier"),("htmlcomment","HTML Comment"),("identifiers","Identifier"),("types1","Types"),("types2","Types"),("scripts","Normal Text"),("scripts_onelinecomment","Comment"),("twolinecomment","Comment")]

parseRules "nosource" = 
  do (attr, result) <- (((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pRegExpr (compileRegex "<\\s*script\\s*language=\"VBScript\"[^>]*>") >>= withAttribute "HTML Tag") >>~ pushContext "aspsource")
                        <|>
                        ((pRegExpr (compileRegex "<\\s*script(\\s|>)") >>= withAttribute "HTML Tag") >>~ pushContext "scripts")
                        <|>
                        ((pRegExpr (compileRegex "<\\s*\\/?\\s*[a-zA-Z_:][a-zA-Z0-9._:-]*") >>= withAttribute "HTML Tag") >>~ pushContext "htmltag")
                        <|>
                        ((pString False "<!--" >>= withAttribute "HTML Comment") >>~ pushContext "htmlcomment"))
     return (attr, result)

parseRules "aspsource" = 
  do (attr, result) <- (((pString False "%>" >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "<\\s*\\/\\s*script\\s*>") >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Comment") >>~ pushContext "asp_onelinecomment")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "doublequotestring")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "singlequotestring")
                        <|>
                        ((pDetectChar False '&' >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "") >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "[0123456789]*\\.\\.\\.[0123456789]*") >>= withAttribute "String"))
                        <|>
                        ((pHlCOct >>= withAttribute "Octal"))
                        <|>
                        ((pHlCHex >>= withAttribute "Hex"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pAnyChar ";()}{:,[]" >>= withAttribute "Other"))
                        <|>
                        ((pKeyword [] >>= withAttribute "Other"))
                        <|>
                        ((pRegExpr (compileRegex "\\belseif\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\belse\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bif\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend if\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bexit function\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bfunction\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend function\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bexit sub\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bsub\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend sub\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bclass\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend class\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bexit do\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bdo\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bloop\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bexit while\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bwhile\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bwend\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bexit for\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bfor\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bnext\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bselect case\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend select\\b") >>= withAttribute "Control Structures"))
                        <|>
                        ((pKeyword ["dim","redim","preserve","const","erase","nothing","set","new","me","function","sub","call","class","private","public","with","randomize","open","close","movenext","execute","eof","not","true","false","or","and","xor"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["select","case","end select","if","then","else","elseif","end if","while","do","until","loop","wend","for","each","to","in","next","exit","continue"] >>= withAttribute "Control Structures"))
                        <|>
                        ((pKeyword ["response","write","redirect","end","request","form","querystring","servervariables","cookies","session","server","createobject","abs","array","asc","atn","cbool","cbyte","ccur","cdate","cdbl","chr","cint","clng","cos","csng","cstr","date","dateadd","DateDiff","DatePart","DateSerial","DateValue","Date","Day","Exp","Filter","Fix","FormatCurrency","FormatDateTime","FormatNumber","FormatPercent","GetObject","Hex","Hour","InputBox","InStr","InStrRev","Int","IsArray","IsDate","IsEmpty","IsNull","IsNumeric","IsObject","Join","LBound","LCase","Left","Len","LoadPicture","Log","LTrim","Mid","Minute","Month","MonthName","MsgBox","Now","Oct","Replace","RGB","Right","Rnd","Round","RTrim","ScriptEngine","ScriptEngineBuildVersion","ScriptEngineMajorVersion","ScriptEngineMinorVersion","Second","Sgn","Sin","Space","Split","Sqr","StrComp","StrReverse","String","Tan","Time","Timer","TimeSerial","TimeValue","Trim","TypeName","UBound","UCase","VarType","Weekday","WeekdayName","Year","Add","AddFolders","BuildPath","Clear","Close","Copy","CopyFile","CopyFolder","CreateFolder","CreateTextFile","Delete","DeleteFile","DeleteFolder","DriveExists","Exists","FileExists","FolderExists","GetAbsolutePathName","GetBaseName","GetDrive","GetDriveName","GetExtensionName","GetFile","GetFileName","GetFolder","GetParentFolderName","GetSpecialFolder","GetTempName","Items","item","Keys","Move","MoveFile","MoveFolder","OpenAsTextStream","OpenTextFile","Raise","Read","ReadAll","ReadLine","Remove","RemoveAll","Skip","SkipLine","Write","WriteBlankLines","WriteLine"] >>= withAttribute "Function")))
     return (attr, result)

parseRules "asp_onelinecomment" = 
  do (attr, result) <- ((pString False "%>" >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules "doublequotestring" = 
  do (attr, result) <- (((pDetect2Chars False '"' '"' >>= withAttribute "Escape Code"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[0-7]{1,3}") >>= withAttribute "Escape Code"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\x[0-9A-Fa-f]{1,2}") >>= withAttribute "Escape Code"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "singlequotestring" = 
  do (attr, result) <- (((pDetect2Chars False '\'' '\'' >>= withAttribute "Escape Code"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "htmltag" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pRegExpr (compileRegex "\\s*=\\s*") >>= withAttribute "Identifier") >>~ pushContext "identifiers"))
     return (attr, result)

parseRules "htmlcomment" = 
  do (attr, result) <- (((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pString False "-->" >>= withAttribute "HTML Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*=\\s*") >>= withAttribute "Normal Text") >>~ pushContext "identifiers"))
     return (attr, result)

parseRules "identifiers" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*#?[a-zA-Z0-9]*") >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Types") >>~ pushContext "types1")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Types") >>~ pushContext "types2"))
     return (attr, result)

parseRules "types1" = 
  do (attr, result) <- (((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Types") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "types2" = 
  do (attr, result) <- (((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Types") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "scripts" = 
  do (attr, result) <- (((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "scripts_onelinecomment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "twolinecomment")
                        <|>
                        ((pKeyword ["select","case","end select","if","then","else","elseif","end if","while","do","until","loop","wend","for","each","to","in","next","exit","continue"] >>= withAttribute "Control Structures"))
                        <|>
                        ((pKeyword ["dim","redim","preserve","const","erase","nothing","set","new","me","function","sub","call","class","private","public","with","randomize","open","close","movenext","execute","eof","not","true","false","or","and","xor"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["response","write","redirect","end","request","form","querystring","servervariables","cookies","session","server","createobject","abs","array","asc","atn","cbool","cbyte","ccur","cdate","cdbl","chr","cint","clng","cos","csng","cstr","date","dateadd","DateDiff","DatePart","DateSerial","DateValue","Date","Day","Exp","Filter","Fix","FormatCurrency","FormatDateTime","FormatNumber","FormatPercent","GetObject","Hex","Hour","InputBox","InStr","InStrRev","Int","IsArray","IsDate","IsEmpty","IsNull","IsNumeric","IsObject","Join","LBound","LCase","Left","Len","LoadPicture","Log","LTrim","Mid","Minute","Month","MonthName","MsgBox","Now","Oct","Replace","RGB","Right","Rnd","Round","RTrim","ScriptEngine","ScriptEngineBuildVersion","ScriptEngineMajorVersion","ScriptEngineMinorVersion","Second","Sgn","Sin","Space","Split","Sqr","StrComp","StrReverse","String","Tan","Time","Timer","TimeSerial","TimeValue","Trim","TypeName","UBound","UCase","VarType","Weekday","WeekdayName","Year","Add","AddFolders","BuildPath","Clear","Close","Copy","CopyFile","CopyFolder","CreateFolder","CreateTextFile","Delete","DeleteFile","DeleteFolder","DriveExists","Exists","FileExists","FolderExists","GetAbsolutePathName","GetBaseName","GetDrive","GetDriveName","GetExtensionName","GetFile","GetFileName","GetFolder","GetParentFolderName","GetSpecialFolder","GetTempName","Items","item","Keys","Move","MoveFile","MoveFolder","OpenAsTextStream","OpenTextFile","Raise","Read","ReadAll","ReadLine","Remove","RemoveAll","Skip","SkipLine","Write","WriteBlankLines","WriteLine"] >>= withAttribute "Function"))
                        <|>
                        ((pString False "<%" >>= withAttribute "Keyword") >>~ pushContext "aspsource")
                        <|>
                        ((pRegExpr (compileRegex "<\\s*\\/\\s*script\\s*>") >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "doublequotestring")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "singlequotestring")
                        <|>
                        ((pHlCOct >>= withAttribute "Octal"))
                        <|>
                        ((pHlCHex >>= withAttribute "Hex"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text"))
                        <|>
                        ((pAnyChar ";()}{:,[]" >>= withAttribute "Other"))
                        <|>
                        ((pKeyword [] >>= withAttribute "Other")))
     return (attr, result)

parseRules "scripts_onelinecomment" = 
  do (attr, result) <- ((pRegExpr (compileRegex "<\\s*\\/\\s*script\\s*>") >>= withAttribute "HTML Tag") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules "twolinecomment" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x