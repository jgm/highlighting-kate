{- This module was generated from data in the Kate syntax highlighting file perl.xml, version 1.20,
   by  Anders Lund (anders@alweb.dk) -}

module Text.Highlighting.Kate.Syntax.Perl ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Perl"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.pl;*.pm"

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
  setState $ st { synStLanguage = "Perl" }
  context <- currentContext <|> (pushContext "normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Perl",["normal"])], synStLanguage = "Perl", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "normal" -> return ()
    "find_quoted" -> return ()
    "find_qqx" -> return ()
    "find_qw" -> return ()
    "ipstring_internal" -> return ()
    "ip_string" -> return ()
    "ip_string_2" -> return ()
    "ip_string_3" -> return ()
    "ip_string_4" -> return ()
    "ip_string_5" -> return ()
    "ip_string_6" -> return ()
    "string" -> return ()
    "string_2" -> return ()
    "string_3" -> return ()
    "string_4" -> return ()
    "string_5" -> return ()
    "string_6" -> return ()
    "find_subst" -> return ()
    "subst_curlybrace_pattern" -> return ()
    "subst_curlybrace_middle" -> return ()
    "subst_curlybrace_replace" -> return ()
    "subst_curlybrace_replace_recursive" -> return ()
    "subst_paren_pattern" -> return ()
    "subst_paren_replace" -> return ()
    "subst_bracket_pattern" -> return ()
    "subst_bracket_replace" -> return ()
    "subst_slash_pattern" -> return ()
    "subst_slash_replace" -> return ()
    "subst_sq_pattern" -> return ()
    "subst_sq_replace" -> return ()
    "tr" -> (popContext >> return ())
    "find_pattern" -> return ()
    "pattern_slash" -> return ()
    "pattern" -> return ()
    "pattern_brace" -> return ()
    "pattern_bracket" -> return ()
    "pattern_paren" -> return ()
    "pattern_sq" -> return ()
    "regex_pattern_internal_rules_1" -> return ()
    "regex_pattern_internal_rules_2" -> return ()
    "regex_pattern_internal" -> return ()
    "regex_pattern_internal_ip" -> return ()
    "pat_ext" -> return ()
    "pat_char_class" -> return ()
    "find_variable" -> (popContext >> return ())
    "find_variable_unsafe" -> (popContext >> return ())
    "var_detect" -> (popContext >> popContext >> return ())
    "var_detect_unsafe" -> (popContext >> popContext >> return ())
    "var_detect_rules" -> (popContext >> popContext >> return ())
    "quote_word" -> return ()
    "quote_word_paren" -> return ()
    "quote_word_brace" -> return ()
    "quote_word_bracket" -> return ()
    "find_here_document" -> (popContext >> return ())
    "here_document" -> return ()
    "here_document_dumb" -> return ()
    "data_handle" -> return ()
    "end_handle" -> return ()
    "Backticked" -> return ()
    "slash_safe_escape" -> (popContext >> return ())
    "package_qualified_blank" -> return ()
    "sub_name_def" -> (popContext >> return ())
    "sub_arg_definition" -> return ()
    "pod" -> return ()
    "comment" -> (popContext >> return ())
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }

withAttribute attr txt = do
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Pragma","Keyword"),("Function","Function"),("Operator","Keyword"),("Data Type","DataType"),("Special Variable","DataType"),("Decimal","DecVal"),("Octal","BaseN"),("Hex","BaseN"),("Float","Float"),("String","String"),("String (interpolated)","String"),("String Special Character","Char"),("Pattern","Others"),("Pattern Internal Operator","Char"),("Pattern Character Class","BaseN"),("Data","Normal"),("Comment","Comment"),("Pod","Comment"),("Nothing","Comment")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("normal","Normal Text"),("find_quoted","Normal Text"),("find_qqx","Normal Text"),("find_qw","Normal Text"),("ipstring_internal","String (interpolated)"),("ip_string","String (interpolated)"),("ip_string_2","String (interpolated)"),("ip_string_3","String (interpolated)"),("ip_string_4","String (interpolated)"),("ip_string_5","String (interpolated)"),("ip_string_6","String (interpolated)"),("string","String"),("string_2","String"),("string_3","String"),("string_4","String"),("string_5","String"),("string_6","String"),("find_subst","Normal Text"),("subst_curlybrace_pattern","Pattern"),("subst_curlybrace_middle","Normal Text"),("subst_curlybrace_replace","String (interpolated)"),("subst_curlybrace_replace_recursive","String (interpolated)"),("subst_paren_pattern","Pattern"),("subst_paren_replace","String (interpolated)"),("subst_bracket_pattern","Pattern"),("subst_bracket_replace","String (interpolated)"),("subst_slash_pattern","Pattern"),("subst_slash_replace","String (interpolated)"),("subst_sq_pattern","Pattern"),("subst_sq_replace","String"),("tr","Pattern"),("find_pattern","Pattern"),("pattern_slash","Pattern"),("pattern","Pattern"),("pattern_brace","Pattern"),("pattern_bracket","Pattern"),("pattern_paren","Pattern"),("pattern_sq","Pattern"),("regex_pattern_internal_rules_1",""),("regex_pattern_internal_rules_2",""),("regex_pattern_internal","Pattern"),("regex_pattern_internal_ip","Pattern"),("pat_ext","Pattern Internal Operator"),("pat_char_class","Pattern Character Class"),("find_variable","Data Type"),("find_variable_unsafe","Data Type"),("var_detect","Data Type"),("var_detect_unsafe","Data Type"),("var_detect_rules","Data Type"),("quote_word","Normal Text"),("quote_word_paren","Normal Text"),("quote_word_brace","Normal Text"),("quote_word_bracket","Normal Text"),("find_here_document","Normal Text"),("here_document","String (interpolated)"),("here_document_dumb","Normal Text"),("data_handle","Data"),("end_handle","Nothing"),("Backticked","String (interpolated)"),("slash_safe_escape","Normal Text"),("package_qualified_blank","Normal Text"),("sub_name_def","Normal Text"),("sub_arg_definition","Normal Text"),("pod","Pod"),("comment","Comment")]

parseRules "normal" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "#!\\/.*") >>= withAttribute "Keyword"))
                        <|>
                        ((pFirstNonSpace >> pString False "__DATA__" >>= withAttribute "Keyword") >>~ pushContext "data_handle")
                        <|>
                        ((pFirstNonSpace >> pString False "__END__" >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bsub\\s+") >>= withAttribute "Keyword") >>~ pushContext "sub_name_def")
                        <|>
                        ((pKeyword ["if","unless","else","elsif","while","until","for","each","foreach","next","last","break","continue","return","use","no","require","my","our","local","BEGIN","END","require","package","sub","do","__END__","__DATA__","__FILE__","__LINE__","__PACKAGE__"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["=","!=","~=","+=","-=","*=","/=","**=","|=","||=","&=","&&=","?=","+","-","*","%","||","&&","|","&","<","<<",">",">>","^","->","=>",".",",",";","::","\\","and","or","not","eq","ne"] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["abs","accept","alarm","atan2","bind","binmode","bless","caller","chdir","chmod","chomp","chop","chown","chr","chroot","close","closedir","connect","cos","crypt","dbmclose","dbmopen","defined","delete","die","dump","endgrent","endhostent","endnetent","endprotoent","endpwent","endservent","eof","eval","exec","exists","exit","exp","fcntl","fileno","flock","fork","format","formline","getc","getgrent","getgrgid","getgrnam","gethostbyaddr","gethostbyname","gethostent","getlogin","getnetbyaddr","getnetbyname","getnetent","getpeername","getpgrp","getppid","getpriority","getprotobyname","getprotobynumber","getprotoent","getpwent","getpwnam","getpwuid","getservbyname","getservbyport","getservent","getsockname","getsockopt","glob","gmtime","goto","grep","hex","import","index","int","ioctl","join","keys","kill","last","lc","lcfirst","length","link","listen","localtime","lock","log","lstat","map","mkdir","msgctl","msgget","msgrcv","msgsnd","oct","open","opendir","ord","pack","package","pipe","pop","pos","print","printf","prototype","push","quotemeta","rand","read","readdir","readline","readlink","recv","redo","ref","rename","reset","return","reverse","rewinddir","rindex","rmdir","scalar","seek","seekdir","select","semctl","semget","semop","send","setgrent","sethostent","setnetent","setpgrp","setpriority","setprotoent","setpwent","setservent","setsockopt","shift","shmctl","shmget","shmread","shmwrite","shutdown","sin","sleep","socket","socketpair","sort","splice","split","sprintf","sqrt","srand","stat","study","sub","substr","symlink","syscall","sysread","sysseek","system","syswrite","tell","telldir","tie","time","times","truncate","uc","ucfirst","umask","undef","unlink","unpack","unshift","untie","utime","values","vec","wait","waitpid","wantarray","warn","write"] >>= withAttribute "Function"))
                        <|>
                        ((pKeyword ["strict","english","warnings","vars","subs","utf8","sigtrap","locale","open","less","integer","filetest","constant","bytes","diagnostics"] >>= withAttribute "Pragma"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\=(?:head[1-6]|over|back|item|for|begin|end|pod)(\\s|$)") >>= withAttribute "Pod") >>~ pushContext "pod")
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "comment")
                        <|>
                        ((pHlCOct >>= withAttribute "Octal") >>~ pushContext "slash_safe_escape")
                        <|>
                        ((pHlCHex >>= withAttribute "Hex") >>~ pushContext "slash_safe_escape")
                        <|>
                        ((pFloat >>= withAttribute "Float") >>~ pushContext "slash_safe_escape")
                        <|>
                        ((pInt >>= withAttribute "Decimal") >>~ pushContext "slash_safe_escape")
                        <|>
                        ((pRegExpr (compileRegex "\\\\([\"'])[^\\1]") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetect2Chars False '&' '\'' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Operator") >>~ pushContext "ip_string")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Operator") >>~ pushContext "string")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Operator") >>~ pushContext "Backticked")
                        <|>
                        ((pRegExpr (compileRegex "(?:[$@]\\S|%[\\w{]|\\*[^\\d\\*{\\$@%=(])") >>= withAttribute "Normal Text") >>~ pushContext "find_variable")
                        <|>
                        ((pRegExpr (compileRegex "<[A-Z0-9_]+>") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\s*<<(?=\\w+|\\s*[\"'])") >>= withAttribute "Operator") >>~ pushContext "find_here_document")
                        <|>
                        ((pRegExpr (compileRegex "\\s*\\}\\s*/") >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\\s*[)]\\s*/") >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\\w+::") >>= withAttribute "Function") >>~ pushContext "sub_name_def")
                        <|>
                        ((pRegExpr (compileRegex "\\w+[=]") >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "\\bq(?=[qwx]?\\s*[^\\w\\s])") >>= withAttribute "Operator") >>~ pushContext "find_quoted")
                        <|>
                        ((pRegExpr (compileRegex "\\bs(?=\\s*[^\\w\\s])") >>= withAttribute "Operator") >>~ pushContext "find_subst")
                        <|>
                        ((pRegExpr (compileRegex "\\b(?:tr|y)\\s*(?=[^\\w\\s\\]})])") >>= withAttribute "Operator") >>~ pushContext "tr")
                        <|>
                        ((pRegExpr (compileRegex "\\b(?:m|qr)(?=\\s*[^\\w\\s\\]})])") >>= withAttribute "Operator") >>~ pushContext "find_pattern")
                        <|>
                        ((pRegExpr (compileRegex "[\\w_]+\\s*/") >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "[<>\"':]/") >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '/' >>= withAttribute "Operator") >>~ pushContext "pattern_slash")
                        <|>
                        ((pRegExpr (compileRegex "-[rwxoRWXOeszfdlpSbctugkTBMAC]") >>= withAttribute "Operator"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text")))
     return (attr, result)

parseRules "find_quoted" = 
  do (attr, result) <- (((pRegExpr (compileRegex "x\\s*(')") >>= withAttribute "Operator") >>~ pushContext "string_6")
                        <|>
                        ((pAnyChar "qx" >>= withAttribute "Operator") >>~ pushContext "find_qqx")
                        <|>
                        ((pDetectChar False 'w' >>= withAttribute "Operator") >>~ pushContext "find_qw")
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Operator") >>~ pushContext "string_2")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Operator") >>~ pushContext "string_3")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Operator") >>~ pushContext "string_4")
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Operator") >>~ pushContext "string_5")
                        <|>
                        ((pRegExpr (compileRegex "([^a-zA-Z0-9_\\s[\\]{}()])") >>= withAttribute "Operator") >>~ pushContext "string_6")
                        <|>
                        ((pRegExpr (compileRegex "\\s+#.*") >>= withAttribute "Comment")))
     return (attr, result)

parseRules "find_qqx" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Operator") >>~ pushContext "ip_string_2")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Operator") >>~ pushContext "ip_string_3")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Operator") >>~ pushContext "ip_string_4")
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Operator") >>~ pushContext "ip_string_5")
                        <|>
                        ((pRegExpr (compileRegex "([^a-zA-Z0-9_\\s[\\]{}()])") >>= withAttribute "Operator") >>~ pushContext "ip_string_6")
                        <|>
                        ((pRegExpr (compileRegex "\\s+#.*") >>= withAttribute "Comment")))
     return (attr, result)

parseRules "find_qw" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Operator") >>~ pushContext "quote_word_paren")
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Operator") >>~ pushContext "quote_word_brace")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Operator") >>~ pushContext "quote_word_bracket")
                        <|>
                        ((pRegExpr (compileRegex "([^a-zA-Z0-9_\\s[\\]{}()])") >>= withAttribute "Operator") >>~ pushContext "quote_word")
                        <|>
                        ((pRegExpr (compileRegex "\\s+#.*") >>= withAttribute "Comment")))
     return (attr, result)

parseRules "ipstring_internal" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[UuLlEtnaefr]") >>= withAttribute "String Special Character"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\.") >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pRegExpr (compileRegex "(?:[\\$@]\\S|%[\\w{])") >>= withAttribute "Normal Text") >>~ pushContext "find_variable_unsafe"))
     return (attr, result)

parseRules "ip_string" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Operator") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "ip_string_2" = 
  do (attr, result) <- (((pRangeDetect '(' ')' >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "ip_string_3" = 
  do (attr, result) <- (((pRangeDetect '{' '}' >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "ip_string_4" = 
  do (attr, result) <- (((pRangeDetect '[' ']' >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "ip_string_5" = 
  do (attr, result) <- (((pRangeDetect '<' '>' >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "ip_string_6" = 
  do (attr, result) <- (((pRegExprDynamic "\\%1" >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pDetectChar True '1' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "string" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String"))
                        <|>
                        ((pDetect2Chars False '\\' '\'' >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Operator") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "string_2" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "String Special Character"))
                        <|>
                        ((pRangeDetect '(' ')' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "string_3" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String"))
                        <|>
                        ((pDetect2Chars False '\\' '}' >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "String Special Character"))
                        <|>
                        ((pRangeDetect '{' '}' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "string_4" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "String Special Character"))
                        <|>
                        ((pRangeDetect '[' ']' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "string_5" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String"))
                        <|>
                        ((pDetect2Chars False '\\' '<' >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetect2Chars False '\\' '>' >>= withAttribute "String"))
                        <|>
                        ((pRangeDetect '<' '>' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "string_6" = 
  do (attr, result) <- (((pDetectIdentifier >>= withAttribute "String"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "String Special Character"))
                        <|>
                        ((pRegExprDynamic "\\%1" >>= withAttribute "String Special Character"))
                        <|>
                        ((pDetectChar True '1' >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "find_subst" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s+#.*") >>= withAttribute "Comment"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Operator") >>~ pushContext "subst_curlybrace_pattern")
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Operator") >>~ pushContext "subst_paren_pattern")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Operator") >>~ pushContext "subst_bracket_pattern")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Operator") >>~ pushContext "subst_sq_pattern")
                        <|>
                        ((pRegExpr (compileRegex "([^\\w\\s[\\]{}()])") >>= withAttribute "Operator") >>~ pushContext "subst_slash_pattern"))
     return (attr, result)

parseRules "subst_curlybrace_pattern" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s+#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((parseRules "regex_pattern_internal_ip"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Operator") >>~ pushContext "subst_curlybrace_middle"))
     return (attr, result)

parseRules "subst_curlybrace_middle" = 
  do (attr, result) <- (((pRegExpr (compileRegex "#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Operator") >>~ pushContext "subst_curlybrace_replace"))
     return (attr, result)

parseRules "subst_curlybrace_replace" = 
  do (attr, result) <- (((parseRules "ipstring_internal"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Normal Text") >>~ pushContext "subst_curlybrace_replace_recursive")
                        <|>
                        ((pRegExpr (compileRegex "\\}[cegimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "subst_curlybrace_replace_recursive" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "String (interpolated)") >>~ pushContext "subst_curlybrace_replace_recursive")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "subst_paren_pattern" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s+#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((parseRules "regex_pattern_internal_ip"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Operator") >>~ pushContext "subst_paren_replace"))
     return (attr, result)

parseRules "subst_paren_replace" = 
  do (attr, result) <- (((parseRules "ipstring_internal"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\)[cegimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "subst_bracket_pattern" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s+#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((parseRules "regex_pattern_internal_ip"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Operator") >>~ pushContext "subst_bracket_replace"))
     return (attr, result)

parseRules "subst_bracket_replace" = 
  do (attr, result) <- (((parseRules "ipstring_internal"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\][cegimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "subst_slash_pattern" = 
  do (attr, result) <- (((pRegExprDynamic "\\$(?=%1)" >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExprDynamic "(%1)" >>= withAttribute "Operator") >>~ pushContext "subst_slash_replace")
                        <|>
                        ((parseRules "regex_pattern_internal_ip")))
     return (attr, result)

parseRules "subst_slash_replace" = 
  do (attr, result) <- (((parseRules "ipstring_internal"))
                        <|>
                        ((pRegExprDynamic "%1[cegimosx]*" >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "subst_sq_pattern" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s+#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((parseRules "regex_pattern_internal"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Operator") >>~ pushContext "subst_sq_replace"))
     return (attr, result)

parseRules "subst_sq_replace" = 
  do (attr, result) <- ((pRegExpr (compileRegex "'[cegimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ()))
     return (attr, result)

parseRules "tr" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\([^)]*\\)\\s*\\(?:[^)]*\\)") >>= withAttribute "Pattern") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "{[^}]*\\}\\s*\\{[^}]*\\}") >>= withAttribute "Pattern") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\[[^}]*\\]\\s*\\[[^\\]]*\\]") >>= withAttribute "Pattern") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "([^a-zA-Z0-9_\\s[\\]{}()]).*\\1.*\\1") >>= withAttribute "Pattern") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "find_pattern" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s+#.*") >>= withAttribute "Comment"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Operator") >>~ pushContext "pattern_brace")
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Operator") >>~ pushContext "pattern_paren")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Operator") >>~ pushContext "pattern_bracket")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Operator") >>~ pushContext "pattern_sq")
                        <|>
                        ((pRegExpr (compileRegex "([^\\w\\s])") >>= withAttribute "Operator") >>~ pushContext "pattern"))
     return (attr, result)

parseRules "pattern_slash" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\$(?=/)") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((parseRules "regex_pattern_internal_ip"))
                        <|>
                        ((pRegExpr (compileRegex "/[cgimosx]*") >>= withAttribute "Operator") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "pattern" = 
  do (attr, result) <- (((pRegExprDynamic "\\$(?=%1)" >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExprDynamic "%1[cgimosx]*" >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "regex_pattern_internal_ip"))
                        <|>
                        ((pRegExprDynamic "\\$(?=\\%1)" >>= withAttribute "Pattern Internal Operator")))
     return (attr, result)

parseRules "pattern_brace" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\}[cgimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "regex_pattern_internal_ip")))
     return (attr, result)

parseRules "pattern_bracket" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\][cgimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "regex_pattern_internal_ip")))
     return (attr, result)

parseRules "pattern_paren" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\)[cgimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "regex_pattern_internal_ip")))
     return (attr, result)

parseRules "pattern_sq" = 
  do (attr, result) <- (((pRegExpr (compileRegex "'[cgimosx]*") >>= withAttribute "Operator") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "regex_pattern_internal")))
     return (attr, result)

parseRules "regex_pattern_internal_rules_1" = 
  do (attr, result) <- (((pFirstNonSpace >> pRegExpr (compileRegex "#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[anDdSsWw]") >>= withAttribute "Pattern Character Class"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[ABbEGLlNUuQdQZz]") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[\\d]+") >>= withAttribute "Special Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\.") >>= withAttribute "Pattern")))
     return (attr, result)

parseRules "regex_pattern_internal_rules_2" = 
  do (attr, result) <- (((pDetect2Chars False '(' '?' >>= withAttribute "Pattern Internal Operator") >>~ pushContext "pat_ext")
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Pattern Internal Operator") >>~ pushContext "pat_char_class")
                        <|>
                        ((pRegExpr (compileRegex "[()?\\^*+|]") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\{[\\d, ]+\\}") >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pDetectChar False '$' >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\s{3,}#.*$") >>= withAttribute "Comment")))
     return (attr, result)

parseRules "regex_pattern_internal" = 
  do (attr, result) <- (((parseRules "regex_pattern_internal_rules_1"))
                        <|>
                        ((parseRules "regex_pattern_internal_rules_2")))
     return (attr, result)

parseRules "regex_pattern_internal_ip" = 
  do (attr, result) <- (((parseRules "regex_pattern_internal_rules_1"))
                        <|>
                        ((pRegExpr (compileRegex "[$@][^\\]\\s{}()|>']") >>= withAttribute "Data Type") >>~ pushContext "find_variable_unsafe")
                        <|>
                        ((parseRules "regex_pattern_internal_rules_2")))
     return (attr, result)

parseRules "pat_ext" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\#[^)]*") >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[:=!><]+") >>= withAttribute "Pattern Internal Operator") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Pattern Internal Operator") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "pat_char_class" = 
  do (attr, result) <- (((pDetectChar False '^' >>= withAttribute "Pattern Internal Operator"))
                        <|>
                        ((pDetect2Chars False '\\' '\\' >>= withAttribute "Pattern Character Class"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Pattern Character Class"))
                        <|>
                        ((pRegExpr (compileRegex "\\[:\\^?[a-z]+:\\]") >>= withAttribute "Pattern Character Class"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Pattern Internal Operator") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "find_variable" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\$[0-9]+") >>= withAttribute "Special Variable") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "[@\\$](?:[\\+\\-_]\\B|ARGV\\b|INC\\b)") >>= withAttribute "Special Variable") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "[%\\$](?:INC\\b|ENV\\b|SIG\\b)") >>= withAttribute "Special Variable") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "\\$\\$[\\$\\w_]") >>= withAttribute "Data Type") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "\\$[#_][\\w_]") >>= withAttribute "Data Type") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "\\$+::") >>= withAttribute "Data Type") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "\\$[^a-zA-Z0-9\\s{][A-Z]?") >>= withAttribute "Special Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[\\$@%]\\{[\\w_]+\\}") >>= withAttribute "Data Type") >>~ pushContext "var_detect")
                        <|>
                        ((pAnyChar "$@%" >>= withAttribute "Data Type") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "\\*[a-zA-Z_]+") >>= withAttribute "Data Type") >>~ pushContext "var_detect")
                        <|>
                        ((pRegExpr (compileRegex "\\*[^a-zA-Z0-9\\s{][A-Z]?") >>= withAttribute "Special Variable"))
                        <|>
                        ((pAnyChar "$@%*" >>= withAttribute "Operator") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "find_variable_unsafe" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\$[0-9]+") >>= withAttribute "Special Variable") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "[@\\$](?:[\\+\\-_]\\B|ARGV\\b|INC\\b)") >>= withAttribute "Special Variable") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "[%\\$](?:INC\\b|ENV\\b|SIG\\b)") >>= withAttribute "Special Variable") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "\\$\\$[\\$\\w_]") >>= withAttribute "Data Type") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "\\$[#_][\\w_]") >>= withAttribute "Data Type") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "\\$+::") >>= withAttribute "Data Type") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "\\$[^a-zA-Z0-9\\s{][A-Z]?") >>= withAttribute "Special Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[\\$@%]\\{[\\w_]+\\}") >>= withAttribute "Data Type") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "[\\$@%]") >>= withAttribute "Data Type") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pRegExpr (compileRegex "\\*\\w+") >>= withAttribute "Data Type") >>~ pushContext "var_detect_unsafe")
                        <|>
                        ((pAnyChar "$@%*" >>= withAttribute "Operator") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "var_detect" = 
  do (attr, result) <- (((parseRules "var_detect_rules"))
                        <|>
                        ((parseRules "slash_safe_escape"))
                        <|>
                        ((popContext >> popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "var_detect_unsafe" = 
  do (attr, result) <- (((parseRules "var_detect_rules"))
                        <|>
                        ((popContext >> popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "var_detect_rules" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[\\w_]+") >>= withAttribute "Data Type"))
                        <|>
                        ((pDetect2Chars False ':' ':' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Operator"))
                        <|>
                        ((pDetect2Chars False '-' '>' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetect2Chars False '+' '+' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Normal Text")))
     return (attr, result)

parseRules "quote_word" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExprDynamic "\\\\%1" >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar True '1' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "quote_word_paren" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "quote_word_brace" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetect2Chars False '\\' '}' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "quote_word_bracket" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Operator") >>~ (popContext >> popContext >> popContext >> return ())))
     return (attr, result)

parseRules "find_here_document" = 
  do (attr, result) <- (((pRegExpr (compileRegex "(\\w+)\\s*;?") >>= withAttribute "Keyword") >>~ pushContext "here_document")
                        <|>
                        ((pRegExpr (compileRegex "\\s*\"([^\"]+)\"\\s*;?") >>= withAttribute "Keyword") >>~ pushContext "here_document")
                        <|>
                        ((pRegExpr (compileRegex "\\s*`([^`]+)`\\s*;?") >>= withAttribute "Keyword") >>~ pushContext "here_document")
                        <|>
                        ((pRegExpr (compileRegex "\\s*'([^']+)'\\s*;?") >>= withAttribute "Keyword") >>~ pushContext "here_document_dumb"))
     return (attr, result)

parseRules "here_document" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "String (interpolated)"))
                        <|>
                        ((pColumn 0 >> pRegExprDynamic "%1" >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\=\\s*<<\\s*[\"']?([A-Z0-9_\\-]+)[\"']?") >>= withAttribute "Keyword") >>~ pushContext "here_document")
                        <|>
                        ((parseRules "ipstring_internal")))
     return (attr, result)

parseRules "here_document_dumb" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pColumn 0 >> pRegExprDynamic "%1" >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text")))
     return (attr, result)

parseRules "data_handle" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "\\=(?:head[1-6]|over|back|item|for|begin|end|pod)\\s+.*") >>= withAttribute "Pod") >>~ pushContext "pod")
                        <|>
                        ((pFirstNonSpace >> pString False "__END__" >>= withAttribute "Keyword") >>~ pushContext "normal"))
     return (attr, result)

parseRules "end_handle" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "\\=(?:head[1-6]|over|back|item|for|begin|end|pod)\\s*.*") >>= withAttribute "Pod") >>~ pushContext "pod")
                        <|>
                        ((pFirstNonSpace >> pString False "__DATA__" >>= withAttribute "Keyword") >>~ pushContext "data_handle"))
     return (attr, result)

parseRules "Backticked" = 
  do (attr, result) <- (((parseRules "ipstring_internal"))
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Operator") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "slash_safe_escape" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*\\]?\\s*/") >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*\\}?\\s*/") >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*\\)?\\s*/") >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pKeyword ["if","unless","else","elsif","while","until","for","each","foreach","next","last","break","continue","return","use","no","require","my","our","local","BEGIN","END","require","package","sub","do","__END__","__DATA__","__FILE__","__LINE__","__PACKAGE__"] >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "package_qualified_blank" = 
  do (attr, result) <- ((pRegExpr (compileRegex "[\\w_]+") >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "sub_name_def" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\w+") >>= withAttribute "Function"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\S") >>= withAttribute "Normal Text") >>~ pushContext "find_variable")
                        <|>
                        ((pRegExpr (compileRegex "\\s*\\(") >>= withAttribute "Normal Text") >>~ pushContext "sub_arg_definition")
                        <|>
                        ((pDetect2Chars False ':' ':' >>= withAttribute "Normal Text"))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "sub_arg_definition" = 
  do (attr, result) <- (((pAnyChar "*$@%" >>= withAttribute "Data Type"))
                        <|>
                        ((pAnyChar "&\\[];" >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Normal Text") >>~ pushContext "slash_safe_escape")
                        <|>
                        ((popContext >> popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "pod" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Pod"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Pod"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\=(?:head[1-6]|over|back|item|for|begin|end|pod)\\s*.*") >>= withAttribute "Pod"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\=cut.*$") >>= withAttribute "Pod") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x