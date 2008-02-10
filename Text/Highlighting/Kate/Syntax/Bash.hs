{- This module was generated from data in the Kate syntax highlighting file bash.xml, version 2.11,
   by  Wilbert Berendsen (wilbert@kde.nl) -}

module Text.Highlighting.Kate.Syntax.Bash ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Bash"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.sh;*.bash;*.ebuild;*.eclass;.bashrc;.bash_profile;.bash_login;.profile"

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
  setState $ st { synStLanguage = "Bash" }
  context <- currentContext <|> (pushContext "Start" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Bash",["Start"])], synStLanguage = "Bash", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Start" -> return ()
    "FindAll" -> return ()
    "FindMost" -> return ()
    "FindComments" -> (popContext >> return ())
    "Comment" -> (popContext >> return ())
    "FindCommentsParen" -> (popContext >> return ())
    "CommentParen" -> (popContext >> return ())
    "FindCommentsBackq" -> (popContext >> return ())
    "CommentBackq" -> (popContext >> return ())
    "FindCommands" -> return ()
    "FindOthers" -> return ()
    "FindStrings" -> return ()
    "FindSubstitutions" -> return ()
    "FindTests" -> return ()
    "ExprDblParen" -> return ()
    "ExprDblParenSubst" -> return ()
    "ExprSubParen" -> return ()
    "ExprBracket" -> return ()
    "ExprDblBracket" -> return ()
    "Group" -> return ()
    "SubShell" -> return ()
    "Assign" -> (popContext >> return ())
    "AssignArray" -> (popContext >> return ())
    "AssignSubscr" -> (popContext >> return ())
    "Subscript" -> return ()
    "FunctionDef" -> (popContext >> return ())
    "VarName" -> (popContext >> return ())
    "ProcessSubst" -> return ()
    "StringSQ" -> return ()
    "StringDQ" -> return ()
    "StringEsc" -> return ()
    "VarBrace" -> return ()
    "VarAlt" -> return ()
    "VarSubst" -> return ()
    "VarSubst2" -> return ()
    "VarSub" -> return ()
    "VarSub2" -> return ()
    "SubstFile" -> return ()
    "SubstCommand" -> return ()
    "SubstBackq" -> return ()
    "Case" -> return ()
    "CaseIn" -> return ()
    "CaseExpr" -> return ()
    "HereDoc" -> return ()
    "HereDocRemainder" -> (popContext >> return ())
    "HereDocQ" -> return ()
    "HereDocNQ" -> return ()
    "HereDocIQ" -> return ()
    "HereDocINQ" -> return ()
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

styles = [("Normal Text","Normal"),("Comment","Comment"),("Keyword","Keyword"),("Control","Keyword"),("Builtin","Keyword"),("Command","Keyword"),("Redirection","Keyword"),("Escape","DataType"),("String SingleQ","String"),("String DoubleQ","String"),("Backquote","Keyword"),("String Transl.","String"),("String Escape","DataType"),("Variable","Others"),("Expression","Others"),("Function","Function"),("Path","Normal"),("Option","Normal"),("Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Start","Normal Text"),("FindAll","Normal Text"),("FindMost","Normal Text"),("FindComments","Normal Text"),("Comment","Comment"),("FindCommentsParen","Normal Text"),("CommentParen","Comment"),("FindCommentsBackq","Normal Text"),("CommentBackq","Comment"),("FindCommands","Normal Text"),("FindOthers","Normal Text"),("FindStrings","Normal Text"),("FindSubstitutions","Normal Text"),("FindTests","Normal Text"),("ExprDblParen","Normal Text"),("ExprDblParenSubst","Normal Text"),("ExprSubParen","Normal Text"),("ExprBracket","Normal Text"),("ExprDblBracket","Normal Text"),("Group","Normal Text"),("SubShell","Normal Text"),("Assign","Normal Text"),("AssignArray","Normal Text"),("AssignSubscr","Normal Text"),("Subscript","Variable"),("FunctionDef","Function"),("VarName","Normal Text"),("ProcessSubst","Normal Text"),("StringSQ","String SingleQ"),("StringDQ","String DoubleQ"),("StringEsc","String SingleQ"),("VarBrace","Error"),("VarAlt","Normal Text"),("VarSubst","Normal Text"),("VarSubst2","Normal Text"),("VarSub","Error"),("VarSub2","Error"),("SubstFile","Normal Text"),("SubstCommand","Normal Text"),("SubstBackq","Normal Text"),("Case","Normal Text"),("CaseIn","Normal Text"),("CaseExpr","Normal Text"),("HereDoc","Normal Text"),("HereDocRemainder","Normal Text"),("HereDocQ","Normal Text"),("HereDocNQ","Normal Text"),("HereDocIQ","Normal Text"),("HereDocINQ","Normal Text")]

parseRules "Start" = 
  do (attr, result) <- ((parseRules "FindAll"))
     return (attr, result)

parseRules "FindAll" = 
  do (attr, result) <- (((parseRules "FindComments"))
                        <|>
                        ((parseRules "FindCommands"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers")))
     return (attr, result)

parseRules "FindMost" = 
  do (attr, result) <- (((parseRules "FindComments"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers")))
     return (attr, result)

parseRules "FindComments" = 
  do (attr, result) <- (((pFirstNonSpace >> pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pRegExpr (compileRegex "[\\s;](?=#)") >>= withAttribute "Normal Text") >>~ pushContext "Comment"))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
     return (attr, result)

parseRules "FindCommentsParen" = 
  do (attr, result) <- (((pFirstNonSpace >> pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "CommentParen")
                        <|>
                        ((pRegExpr (compileRegex "[\\s;](?=#)") >>= withAttribute "Normal Text") >>~ pushContext "CommentParen"))
     return (attr, result)

parseRules "CommentParen" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[^)](?=\\))") >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "FindCommentsBackq" = 
  do (attr, result) <- (((pFirstNonSpace >> pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "CommentBackq")
                        <|>
                        ((pRegExpr (compileRegex "[\\s;](?=#)") >>= withAttribute "Normal Text") >>~ pushContext "CommentBackq"))
     return (attr, result)

parseRules "CommentBackq" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[^`](?=`)") >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "FindCommands" = 
  do (attr, result) <- (((pDetect2Chars False '(' '(' >>= withAttribute "Keyword") >>~ pushContext "ExprDblParen")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\[\\[(?=($|\\s))") >>= withAttribute "Keyword") >>~ pushContext "ExprDblBracket")
                        <|>
                        ((pRegExpr (compileRegex "\\s\\[\\[(?=($|\\s))") >>= withAttribute "Keyword") >>~ pushContext "ExprDblBracket")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\[(?=($|\\s))") >>= withAttribute "Builtin") >>~ pushContext "ExprBracket")
                        <|>
                        ((pRegExpr (compileRegex "\\s\\[(?=($|\\s))") >>= withAttribute "Builtin") >>~ pushContext "ExprBracket")
                        <|>
                        ((pRegExpr (compileRegex "\\{(?=($|\\s))") >>= withAttribute "Keyword") >>~ pushContext "Group")
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Keyword") >>~ pushContext "SubShell")
                        <|>
                        ((pRegExpr (compileRegex "\\bdo(?![\\w$+-])") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bdone(?![\\w$+-])") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bif(?![\\w$+-])") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bfi(?![\\w$+-])") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bcase(?![\\w$+-])") >>= withAttribute "Keyword") >>~ pushContext "Case")
                        <|>
                        ((pRegExpr (compileRegex "-[A-Za-z0-9][A-Za-z0-9_]*") >>= withAttribute "Option"))
                        <|>
                        ((pRegExpr (compileRegex "--[a-z][A-Za-z0-9_-]*") >>= withAttribute "Option"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[A-Za-z_][A-Za-z0-9_]*\\+?=") >>= withAttribute "Variable") >>~ pushContext "Assign")
                        <|>
                        ((pRegExpr (compileRegex "\\b[A-Za-z_][A-Za-z0-9_]*(?=\\[.+\\]\\+?=)") >>= withAttribute "Variable") >>~ pushContext "AssignSubscr")
                        <|>
                        ((pString False ":()" >>= withAttribute "Function"))
                        <|>
                        ((pRegExpr (compileRegex "\\bfunction\\b") >>= withAttribute "Keyword") >>~ pushContext "FunctionDef")
                        <|>
                        ((pKeyword " \n\t()!+,<=>&*;?|~\\`" ["else","for","function","in","select","until","while","elif","then","set"] >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\.(?=\\s)") >>= withAttribute "Builtin"))
                        <|>
                        ((pKeyword " \n\t()!+,<=>&*;?|~\\`" [":","source","alias","bg","bind","break","builtin","cd","caller","command","compgen","complete","continue","dirs","disown","echo","enable","eval","exec","exit","fc","fg","getopts","hash","help","history","jobs","kill","let","logout","popd","printf","pushd","pwd","return","set","shift","shopt","suspend","test","time","times","trap","type","ulimit","umask","unalias","wait"] >>= withAttribute "Builtin"))
                        <|>
                        ((pKeyword " \n\t()!+,<=>&*;?|~\\`" ["arch","awk","bash","bunzip2","bzcat","bzcmp","bzdiff","bzegrep","bzfgrep","bzgrep","bzip2","bzip2recover","bzless","bzmore","cat","chattr","chgrp","chmod","chown","chvt","cp","date","dd","deallocvt","df","dir","dircolors","dmesg","dnsdomainname","domainname","du","dumpkeys","echo","ed","egrep","false","fgconsole","fgrep","fuser","gawk","getkeycodes","gocr","grep","groff","groups","gunzip","gzexe","gzip","hostname","igawk","install","kbd_mode","kbdrate","killall","last","lastb","link","ln","loadkeys","loadunimap","login","ls","lsattr","lsmod","lsmod.old","mapscrn","mesg","mkdir","mkfifo","mknod","mktemp","more","mount","mv","nano","netstat","nisdomainname","nroff","openvt","pgawk","pidof","ping","ps","pstree","pwd","rbash","readlink","red","resizecons","rm","rmdir","run-parts","sash","sed","setfont","setkeycodes","setleds","setmetamode","setserial","sh","showkey","shred","sleep","ssed","stat","stty","su","sync","tar","tempfile","touch","troff","true","umount","uname","unicode_start","unicode_stop","unlink","utmpdump","uuidgen","vdir","wall","wc","ypdomainname","zcat","zcmp","zdiff","zegrep","zfgrep","zforce","zgrep","zless","zmore","znew","zsh","aclocal","aconnect","aplay","apm","apmsleep","apropos","ar","arecord","as","as86","autoconf","autoheader","automake","awk","basename","bc","bison","c++","cal","cat","cc","cdda2wav","cdparanoia","cdrdao","cd-read","cdrecord","chfn","chgrp","chmod","chown","chroot","chsh","clear","cmp","co","col","comm","cp","cpio","cpp","cut","dc","dd","df","diff","diff3","dir","dircolors","directomatic","dirname","du","env","expr","fbset","file","find","flex","flex++","fmt","free","ftp","funzip","fuser","g++","gawk","gc","gcc","gdb","getent","getopt","gettext","gettextize","gimp","gimp-remote","gimptool","gmake","gs","head","hexdump","id","install","join","kill","killall","ld","ld86","ldd","less","lex","ln","locate","lockfile","logname","lp","lpr","ls","lynx","m4","make","man","mkdir","mknod","msgfmt","mv","namei","nasm","nawk","nice","nl","nm","nm86","nmap","nohup","nop","od","passwd","patch","pcregrep","pcretest","perl","perror","pidof","pr","printf","procmail","prune","ps2ascii","ps2epsi","ps2frag","ps2pdf","ps2ps","psbook","psmerge","psnup","psresize","psselect","pstops","rcs","rev","rm","scp","sed","seq","setterm","shred","size","size86","skill","slogin","snice","sort","sox","split","ssh","ssh-add","ssh-agent","ssh-keygen","ssh-keyscan","stat","strings","strip","sudo","suidperl","sum","tac","tail","tee","test","tr","uniq","unlink","unzip","updatedb","updmap","uptime","users","vmstat","w","wc","wget","whatis","whereis","which","who","whoami","write","xargs","yacc","yes","zip","zsoelim","dcop","kdialog","kfile","xhost","xmodmap","xset"] >>= withAttribute "Command"))
                        <|>
                        ((pKeyword " \n\t()!+,<=>&*;?|~\\`" ["export","unset","declare","typeset","local","read","readonly"] >>= withAttribute "Builtin") >>~ pushContext "VarName")
                        <|>
                        ((pRegExpr (compileRegex "\\d*<<<") >>= withAttribute "Redirection"))
                        <|>
                        ((pString False "<<" >>= withAttribute "Redirection") >>~ pushContext "HereDoc")
                        <|>
                        ((pRegExpr (compileRegex "[<>]\\(") >>= withAttribute "Redirection") >>~ pushContext "ProcessSubst")
                        <|>
                        ((pRegExpr (compileRegex "([0-9]*(>{1,2}|<)(&[0-9]+-?)?|&>|>&|[0-9]*<>)") >>= withAttribute "Redirection"))
                        <|>
                        ((pRegExpr (compileRegex "([|&])\\1?") >>= withAttribute "Control"))
                        <|>
                        ((pRegExpr (compileRegex "[A-Za-z_:][A-Za-z0-9_:#%@-]*\\s*\\(\\)") >>= withAttribute "Function")))
     return (attr, result)

parseRules "FindOthers" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\[][;\\\\$`{}()|&<>* ]") >>= withAttribute "Escape"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\$") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\{(?!(\\s|$))\\S*\\}") >>= withAttribute "Escape"))
                        <|>
                        ((pRegExpr (compileRegex "([\\w_@.%*?+-]|\\\\ )*(?=/)") >>= withAttribute "Path"))
                        <|>
                        ((pRegExpr (compileRegex "~\\w*") >>= withAttribute "Path"))
                        <|>
                        ((pRegExpr (compileRegex "/([\\w_@.%*?+-]|\\\\ )*(?=([\\s/):;$`'\"]|$))") >>= withAttribute "Path")))
     return (attr, result)

parseRules "FindStrings" = 
  do (attr, result) <- (((pDetect2Chars False '\\' '\'' >>= withAttribute "Escape"))
                        <|>
                        ((pDetect2Chars False '\\' '"' >>= withAttribute "Escape"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String SingleQ") >>~ pushContext "StringSQ")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String DoubleQ") >>~ pushContext "StringDQ")
                        <|>
                        ((pDetect2Chars False '$' '\'' >>= withAttribute "String SingleQ") >>~ pushContext "StringEsc")
                        <|>
                        ((pDetect2Chars False '$' '"' >>= withAttribute "String Transl.") >>~ pushContext "StringDQ"))
     return (attr, result)

parseRules "FindSubstitutions" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\$[A-Za-z_][A-Za-z0-9_]*\\[") >>= withAttribute "Variable") >>~ pushContext "Subscript")
                        <|>
                        ((pRegExpr (compileRegex "\\$[A-Za-z_][A-Za-z0-9_]*") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$[*@#?$!_0-9-]") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{[*@#?$!_0-9-]\\}") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{#[A-Za-z_][A-Za-z0-9_]*(\\[[*@]\\])?\\}") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{![A-Za-z_][A-Za-z0-9_]*(\\[[*@]\\]|[*@])?\\}") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{[A-Za-z_][A-Za-z0-9_]*") >>= withAttribute "Variable") >>~ pushContext "VarBrace")
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{[*@#?$!_0-9-](?=[:#%/=?+-])") >>= withAttribute "Variable") >>~ pushContext "VarBrace")
                        <|>
                        ((pString False "$((" >>= withAttribute "Variable") >>~ pushContext "ExprDblParenSubst")
                        <|>
                        ((pString False "$(<" >>= withAttribute "Redirection") >>~ pushContext "SubstFile")
                        <|>
                        ((pString False "$(" >>= withAttribute "Variable") >>~ pushContext "SubstCommand")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Backquote") >>~ pushContext "SubstBackq")
                        <|>
                        ((pRegExpr (compileRegex "\\\\[`$\\\\]") >>= withAttribute "Escape")))
     return (attr, result)

parseRules "FindTests" = 
  do (attr, result) <- (((pRegExpr (compileRegex "-[abcdefghkprstuwxOGLSNozn](?=\\s)") >>= withAttribute "Expression"))
                        <|>
                        ((pRegExpr (compileRegex "-([no]t|ef)(?=\\s)") >>= withAttribute "Expression"))
                        <|>
                        ((pRegExpr (compileRegex "([!=]=?|[><])(?=\\s)") >>= withAttribute "Expression"))
                        <|>
                        ((pRegExpr (compileRegex "-(eq|ne|[gl][te])(?=\\s)") >>= withAttribute "Expression")))
     return (attr, result)

parseRules "ExprDblParen" = 
  do (attr, result) <- (((pDetect2Chars False ')' ')' >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "ExprSubParen")
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "ExprDblParenSubst" = 
  do (attr, result) <- (((pDetect2Chars False ')' ')' >>= withAttribute "Variable") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "ExprSubParen")
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "ExprSubParen" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "ExprSubParen")
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "ExprBracket" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s\\](?=($|[\\s;|&]))") >>= withAttribute "Builtin") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\](?=($|[\\s;|&]))") >>= withAttribute "Builtin") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "ExprSubParen")
                        <|>
                        ((parseRules "FindTests"))
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "ExprDblBracket" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s\\]\\](?=($|[\\s;|&]))") >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\]\\](?=($|[\\s;|&]))") >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "ExprSubParen")
                        <|>
                        ((parseRules "FindTests"))
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "Group" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindAll")))
     return (attr, result)

parseRules "SubShell" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindAll")))
     return (attr, result)

parseRules "Assign" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Variable") >>~ pushContext "AssignArray")
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers"))
                        <|>
                        ((pRegExpr (compileRegex "[\\w:,+_./-]") >>= withAttribute "Normal Text"))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "AssignArray" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Variable") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Variable") >>~ pushContext "Subscript")
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Variable") >>~ pushContext "Assign")
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "AssignSubscr" = 
  do (attr, result) <- (((pDetectChar False '[' >>= withAttribute "Variable") >>~ pushContext "Subscript")
                        <|>
                        ((pDetect2Chars False '+' '=' >>= withAttribute "Variable") >>~ pushContext "Assign")
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Variable") >>~ pushContext "Assign")
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers"))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Subscript" = 
  do (attr, result) <- (((pDetectChar False ']' >>= withAttribute "Variable") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers")))
     return (attr, result)

parseRules "FunctionDef" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s+[A-Za-z_:][A-Za-z0-9_:#%@-]*(\\s*\\(\\))?") >>= withAttribute "Function") >>~ (popContext >> return ()))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "VarName" = 
  do (attr, result) <- (((pRegExpr (compileRegex "-[A-Za-z0-9]+") >>= withAttribute "Option"))
                        <|>
                        ((pRegExpr (compileRegex "--[a-z][A-Za-z0-9_-]*") >>= withAttribute "Option"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[A-Za-z_][A-Za-z0-9_]*") >>= withAttribute "Variable"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Variable") >>~ pushContext "Subscript")
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Variable") >>~ pushContext "Assign")
                        <|>
                        ((parseRules "FindMost"))
                        <|>
                        ((pRegExpr (compileRegex "[^]})|;`&><]") >>= withAttribute "Normal Text"))
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "ProcessSubst" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Redirection") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindCommentsParen"))
                        <|>
                        ((parseRules "FindCommands"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers")))
     return (attr, result)

parseRules "StringSQ" = 
  do (attr, result) <- ((pDetectChar False '\'' >>= withAttribute "String SingleQ") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "StringDQ" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "String DoubleQ") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[`\"\\\\$\\n]") >>= withAttribute "String Escape"))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules "StringEsc" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "String SingleQ") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\\\[abefnrtv\\\\']") >>= withAttribute "String Escape"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\([0-7]{1,3}|x[A-Fa-f0-9]{1,2}|c.)") >>= withAttribute "String Escape")))
     return (attr, result)

parseRules "VarBrace" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Variable") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Variable") >>~ pushContext "Subscript")
                        <|>
                        ((pRegExpr (compileRegex "(:?[-=?+]|##?|%%?)") >>= withAttribute "Variable") >>~ pushContext "VarAlt")
                        <|>
                        ((pRegExpr (compileRegex "//?") >>= withAttribute "Variable") >>~ pushContext "VarSubst")
                        <|>
                        ((pDetectChar False ':' >>= withAttribute "Variable") >>~ pushContext "VarSub"))
     return (attr, result)

parseRules "VarAlt" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Variable") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules "VarSubst" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Variable") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '/' >>= withAttribute "Variable") >>~ pushContext "VarSubst2")
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules "VarSubst2" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Variable") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules "VarSub" = 
  do (attr, result) <- (((pDetectChar False ':' >>= withAttribute "Variable") >>~ pushContext "VarSub2")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Variable") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[A-Za-z_][A-Za-z0-9_]*") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[0-9]+(?=[:}])") >>= withAttribute "Variable"))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules "VarSub2" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Variable") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[A-Za-z_][A-Za-z0-9_]*") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[0-9](?=[:}])") >>= withAttribute "Variable"))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules "SubstFile" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Redirection") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindCommentsParen"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers")))
     return (attr, result)

parseRules "SubstCommand" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Variable") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindCommentsParen"))
                        <|>
                        ((parseRules "FindCommands"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers")))
     return (attr, result)

parseRules "SubstBackq" = 
  do (attr, result) <- (((pDetectChar False '`' >>= withAttribute "Backquote") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindCommentsBackq"))
                        <|>
                        ((parseRules "FindCommands"))
                        <|>
                        ((parseRules "FindStrings"))
                        <|>
                        ((parseRules "FindSubstitutions"))
                        <|>
                        ((parseRules "FindOthers")))
     return (attr, result)

parseRules "Case" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\sin\\b") >>= withAttribute "Keyword") >>~ pushContext "CaseIn")
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "CaseIn" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\besac(?=$|[\\s;)])") >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Keyword") >>~ pushContext "CaseExpr")
                        <|>
                        ((pAnyChar "(|" >>= withAttribute "Keyword"))
                        <|>
                        ((parseRules "FindMost")))
     return (attr, result)

parseRules "CaseExpr" = 
  do (attr, result) <- (((pDetect2Chars False ';' ';' >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindAll")))
     return (attr, result)

parseRules "HereDoc" = 
  do (attr, result) <- (((pRegExpr (compileRegex "(<<-\\s*\"([^|&;()<>\\s]+)\")") >>= withAttribute "Redirection") >>~ pushContext "HereDocIQ")
                        <|>
                        ((pRegExpr (compileRegex "(<<-\\s*'([^|&;()<>\\s]+)')") >>= withAttribute "Redirection") >>~ pushContext "HereDocIQ")
                        <|>
                        ((pRegExpr (compileRegex "(<<-\\s*\\\\([^|&;()<>\\s]+))") >>= withAttribute "Redirection") >>~ pushContext "HereDocIQ")
                        <|>
                        ((pRegExpr (compileRegex "(<<-\\s*([^|&;()<>\\s]+))") >>= withAttribute "Redirection") >>~ pushContext "HereDocINQ")
                        <|>
                        ((pRegExpr (compileRegex "(<<\\s*\"([^|&;()<>\\s]+)\")") >>= withAttribute "Redirection") >>~ pushContext "HereDocQ")
                        <|>
                        ((pRegExpr (compileRegex "(<<\\s*'([^|&;()<>\\s]+)')") >>= withAttribute "Redirection") >>~ pushContext "HereDocQ")
                        <|>
                        ((pRegExpr (compileRegex "(<<\\s*\\\\([^|&;()<>\\s]+))") >>= withAttribute "Redirection") >>~ pushContext "HereDocQ")
                        <|>
                        ((pRegExpr (compileRegex "(<<\\s*([^|&;()<>\\s]+))") >>= withAttribute "Redirection") >>~ pushContext "HereDocNQ")
                        <|>
                        ((pString False "<<" >>= withAttribute "Redirection") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "HereDocRemainder" = 
  do (attr, result) <- ((parseRules "FindAll"))
     return (attr, result)

parseRules "HereDocQ" = 
  do (attr, result) <- (((pRegExprDynamic "%1" >>= withAttribute "Redirection") >>~ pushContext "HereDocRemainder")
                        <|>
                        ((pColumn 0 >> pRegExprDynamic "%2\\b" >>= withAttribute "Redirection") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "HereDocNQ" = 
  do (attr, result) <- (((pRegExprDynamic "%1" >>= withAttribute "Redirection") >>~ pushContext "HereDocRemainder")
                        <|>
                        ((pColumn 0 >> pRegExprDynamic "%2\\b" >>= withAttribute "Redirection") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules "HereDocIQ" = 
  do (attr, result) <- (((pRegExprDynamic "%1" >>= withAttribute "Redirection") >>~ pushContext "HereDocRemainder")
                        <|>
                        ((pColumn 0 >> pRegExprDynamic "\\t*%2\\b" >>= withAttribute "Redirection") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "HereDocINQ" = 
  do (attr, result) <- (((pRegExprDynamic "%1" >>= withAttribute "Redirection") >>~ pushContext "HereDocRemainder")
                        <|>
                        ((pColumn 0 >> pRegExprDynamic "\\t*%2\\b" >>= withAttribute "Redirection") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindSubstitutions")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
