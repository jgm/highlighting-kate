{- This module was generated from data in the Kate syntax highlighting file sql-mysql.xml, version 1.09,
   by  Shane Wright (me@shanewright.co.uk) -}

module Text.Highlighting.Kate.Syntax.SqlMysql ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "SQL (MySQL)"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.sql;*.SQL"

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
  setState $ st { synStLanguage = "SQL (MySQL)" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("SQL (MySQL)",["Normal"])], synStLanguage = "SQL (MySQL)", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = False, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t(),%&;?[]{}\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String" -> return ()
    "String2" -> return ()
    "Name" -> return ()
    "SingleLineComment" -> (popContext >> return ())
    "MultiLineComment" -> return ()
    "Preprocessor" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Operator","Normal"),("Function","Function"),("Data Type","DataType"),("Decimal","DecVal"),("Float","Float"),("String","String"),("Name","String"),("String Char","Char"),("Comment","Comment"),("Symbol","Char"),("Preprocessor","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String","String"),("String2","String"),("Name","Name"),("SingleLineComment","Comment"),("MultiLineComment","Comment"),("Preprocessor","Preprocessor")]

parseRules "Normal" = 
  do (attr, result) <- (((pRegExpr (compileRegex "SET(?=\\s*\\()") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "\\bCHARACTER SET\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["ACCESS","ADD","ALL","ALTER","ANALYZE","AND","AS","ASC","AUTO_INCREMENT","BDB","BERKELEYDB","BETWEEN","BOTH","BY","CASCADE","CASE","CHANGE","CHARSET","COLUMN","COLUMNS","CONSTRAINT","CREATE","CROSS","CURRENT_DATE","CURRENT_TIME","CURRENT_TIMESTAMP","DATABASE","DATABASES","DAY_HOUR","DAY_MINUTE","DAY_SECOND","DEC","DEFAULT","DELAYED","DELETE","DESC","DESCRIBE","DISTINCT","DISTINCTROW","DROP","ELSE","ENCLOSED","ESCAPED","EXISTS","EXPLAIN","FIELDS","FOR","FOREIGN","FROM","FULLTEXT","FUNCTION","GRANT","GROUP","HAVING","HIGH_PRIORITY","IF","IGNORE","IN","INDEX","INFILE","INNER","INNODB","INSERT","INTERVAL","INTO","IS","JOIN","KEY","KEYS","KILL","LEADING","LEFT","LIKE","LIMIT","LINES","LOAD","LOCK","LOW_PRIORITY","MASTER_SERVER_ID","MATCH","MRG_MYISAM","NATURAL","NATIONAL","NOT","NULL","NUMERIC","ON","OPTIMIZE","OPTION","OPTIONALLY","OR","ORDER","OUTER","OUTFILE","PARTIAL","PRECISION","PRIMARY","PRIVILEGES","PROCEDURE","PURGE","READ","REFERENCES","REGEXP","RENAME","REPLACE","REQUIRE","RESTRICT","RETURNS","REVOKE","RIGHT","RLIKE","SELECT","SET","SHOW","SONAME","SQL_BIG_RESULT","SQL_CALC_FOUND_ROWS","SQL_SMALL_RESULT","SSL","STARTING","STRAIGHT_JOIN","STRIPED","TABLE","TABLES","TERMINATED","THEN","TO","TRAILING","TRUNCATE","TYPE","UNION","UNIQUE","UNLOCK","UNSIGNED","UPDATE","USAGE","USE","USER_RESOURCES","USING","VALUES","VARYING","WHEN","WHERE","WITH","WRITE","XOR","YEAR_MONTH","ZEROFILL"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["+","-","*","/","||","=","!=","<>","<","<=",">",">=","~=","^=",":=","=>","**",".."] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["ASCII","ORD","CONV","BIN","OCT","HEX","CHAR","CONCAT","CONCAT_WS","LENGTH","OCTET_LENGTH","CHAR_LENGTH","CHARACTER_LENGTH","BIT_LENGTH","LOCATE","POSITION","INSTR","LPAD","RPAD","LEFT","RIGHT","SUBSTRING","SUBSTRING_INDEX","MID","LTRIM","RTRIM","TRIM","SOUNDEX","SPACE","REPLACE","REPEAT","REVERSE","INSERT","ELT","FIELD","FIND_IN_SET","MAKE_SET","EXPORT_SET","LCASE","LOWER","UCASE","UPPER","LOAD_FILE","QUOTE","ABS","SIGN","MOD","FLOOR","CEILING","ROUND","EXP","LN","LOG","LOG2","LOG10","POW","POWER","SQRT","PI","COS","SIN","TAN","ACOS","ASIN","ATAN","ATAN2","COT","RAND","LEAST","GREATEST","DEGREES","RADIANS","DAYOFWEEK","WEEKDAY","DAYOFMONTH","DAYOFYEAR","MONTH","DAYNAME","MONTHNAME","QUARTER","WEEK","YEAR","YEARWEEK","HOUR","MINUTE","SECOND","PERIOD_ADD","PERIOD_DIFF","DATE_ADD","DATE_SUB","ADDDATE","SUBDATE","EXTRACT","TO_DAYS","FROM_DAYS","DATE_FORMAT","TIME_FORMAT","CURDATE","CURRENT_DATE","CURTIME","CURRENT_TIME","NOW","SYSDATE","CURRENT_TIMESTAMP","UNIX_TIMESTAMP","FROM_UNIXTIME","SEC_TO_TIME","TIME_TO_SEC","CAST","CONVERT","BIT_COUNT","DATABASE","USER","SYSTEM_USER","SESSION_USER","PASSWORD","ENCRYPT","ENCODE","DECODE","MD5","SHA1","SHA","AES_ENCRYPT","AES_DECRYPT","DES_ENCRYPT","DES_DECRYPT","LAST_INSERT_ID","FORMAT","VERSION","CONNECTION_ID","GET_LOCK","RELEASE_LOCK","IS_FREE_LOCK","BENCHMARK","INET_NTOA","INET_ATON","MASTER_POS_WAIT","FOUND_ROWS","COUNT","AVG","MIN","MAX","SUM","STD","STDDEV","BIT_OR","BIT_AND"] >>= withAttribute "Function"))
                        <|>
                        ((pKeyword ["CHAR","CHARACTER","VARCHAR","BINARY","VARBINARY","TINYBLOB","MEDIUMBLOB","BLOB","LONGBLOB","TINYTEXT","MEDIUMTEXT","TEXT","LONGTEXT","ENUM","BIT","BOOL","BOOLEAN","TINYINT","SMALLINT","MEDIUMINT","MIDDLEINT","INT","INTEGER","BIGINT","FLOAT","DOUBLE","REAL","DECIMAL","DEC","FIXED","NUMERIC","LONG","SERIAL","DATE","DATETIME","TIME","TIMESTAMP","YEAR"] >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%bulk_exceptions\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%bulk_rowcount\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%found\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%isopen\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%notfound\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%rowcount\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%rowtype\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "%type\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String2")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Name") >>~ pushContext "Name")
                        <|>
                        ((pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "MultiLineComment")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "rem\\b") >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pAnyChar ":&" >>= withAttribute "Symbol"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "/$") >>= withAttribute "Symbol"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "@@?[^@ \\t\\r\\n]") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor")
                        <|>
                        ((pDetectChar False '.' >>= withAttribute "String Char")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '&' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "String2" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '&' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Name" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Name") >>~ (popContext >> return ()))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Name") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "SingleLineComment" = 
  pzero

parseRules "MultiLineComment" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Preprocessor" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x
