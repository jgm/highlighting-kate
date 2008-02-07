{- This module was generated from data in the Kate syntax highlighting file sql-postgresql.xml, version 1.08,
   by  Shane Wright (me@shanewright.co.uk) -}

module Text.Highlighting.Kate.Syntax.SqlPostgresql ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "SQL (PostgreSQL)"

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
  setState $ st { synStLanguage = "SQL (PostgreSQL)" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("SQL (PostgreSQL)",["Normal"])], synStLanguage = "SQL (PostgreSQL)", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = False, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t(),;[]{}\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String" -> return ()
    "SingleLineComment" -> (popContext >> return ())
    "MultiLineComment" -> return ()
    "Identifier" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Operator","Normal"),("Function","Function"),("Data Type","DataType"),("Decimal","DecVal"),("Float","Float"),("String","String"),("String Char","Char"),("Comment","Comment"),("Identifier","Others"),("Symbol","Char"),("Preprocessor","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String","String"),("SingleLineComment","Comment"),("MultiLineComment","Comment"),("Identifier","Identifier"),("Preprocessor","Preprocessor")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword ["ABORT","ACCESS","ACTION","ADD","ADMIN","AFTER","AGGREGATE","ALIAS","ALL","ALLOCATE","ALTER","ANALYSE","ANALYZE","ANY","ARE","AS","ASC","ASENSITIVE","ASSERTION","ASSIGNMENT","ASYMMETRIC","AT","ATOMIC","AUTHORIZATION","BACKWARD","BEFORE","BEGIN","BETWEEN","BINARY","BOTH","BREADTH","BY","C","CACHE","CALL","CALLED","CARDINALITY","CASCADE","CASCADED","CASE","CAST","CATALOG","CATALOG_NAME","CHAIN","CHAR_LENGTH","CHARACTER_LENGTH","CHARACTER_SET_CATALOG","CHARACTER_SET_NAME","CHARACTER_SET_SCHEMA","CHARACTERISTICS","CHECK","CHECKED","CHECKPOINT","CLASS","CLASS_ORIGIN","CLOB","CLOSE","CLUSTER","COALESCE","COBOL","COLLATE","COLLATION","COLLATION_CATALOG","COLLATION_NAME","COLLATION_SCHEMA","COLUMN","COLUMN_NAME","COMMAND_FUNCTION","COMMAND_FUNCTION_CODE","COMMENT","COMMIT","COMMITTED","COMPLETION","CONDITION_NUMBER","CONNECT","CONNECTION","CONNECTION_NAME","CONSTRAINT","CONSTRAINT_CATALOG","CONSTRAINT_NAME","CONSTRAINT_SCHEMA","CONSTRAINTS","CONSTRUCTOR","CONTAINS","CONTINUE","CONVERT","COPY","CORRESPONDING","COUNT","CREATE","CREATEDB","CREATEUSER","CROSS","CUBE","CURRENT","CURRENT_DATE","CURRENT_PATH","CURRENT_ROLE","CURRENT_TIME","CURRENT_TIMESTAMP","CURRENT_USER","CURSOR","CURSOR_NAME","CYCLE","DATA","DATABASE","DATE","DATETIME_INTERVAL_CODE","DATETIME_INTERVAL_PRECISION","DAY","DEALLOCATE","DEC","DECIMAL","DECLARE","DEFAULT","DEFERRABLE","DEFERRED","DEFINED","DEFINER","DELETE","DELIMITERS","DEPTH","DEREF","DESC","DESCRIBE","DESCRIPTOR","DESTROY","DESTRUCTOR","DETERMINISTIC","DIAGNOSTICS","DICTIONARY","DISCONNECT","DISPATCH","DISTINCT","DO","DOMAIN","DOUBLE","DROP","DYNAMIC","DYNAMIC_FUNCTION","DYNAMIC_FUNCTION_CODE","EACH","ELSE","ENCODING","ENCRYPTED","END","END-EXEC","EQUALS","ESCAPE","EVERY","EXCEPT","EXCEPTION","EXCLUSIVE","EXEC","EXECUTE","EXISTING","EXISTS","EXPLAIN","EXTERNAL","FETCH","FINAL","FIRST","FOR","FORCE","FOREIGN","FORTRAN","FORWARD","FOUND","FREE","FREEZE","FROM","FULL","FUNCTION","G","GENERAL","GENERATED","GET","GLOBAL","GO","GOTO","GRANT","GRANTED","GROUP","GROUPING","HANDLER","HAVING","HIERARCHY","HOLD","HOST","HOUR","IDENTITY","IGNORE","ILIKE","IMMEDIATE","IMMUTABLE","IMPLEMENTATION","IN","INCREMENT","INDEX","INDICATOR","INFIX","INHERITS","INITIALIZE","INITIALLY","INNER","INOUT","INPUT","INSENSITIVE","INSERT","INSTANCE","INSTANTIABLE","INSTEAD","INTERSECT","INTERVAL","INTO","INVOKER","IS","ISNULL","ISOLATION","ITERATE","JOIN","K","KEY","KEY_MEMBER","KEY_TYPE","LANCOMPILER","LANGUAGE","LARGE","LAST","LATERAL","LEADING","LEFT","LENGTH","LESS","LEVEL","LIKE","LIMIT","LISTEN","LOAD","LOCAL","LOCALTIME","LOCALTIMESTAMP","LOCATION","LOCATOR","LOCK","LOWER","M","MAP","MATCH","MAX","MAXVALUE","MESSAGE_LENGTH","MESSAGE_OCTET_LENGTH","MESSAGE_TEXT","METHOD","MIN","MINUTE","MINVALUE","MOD","MODE","MODIFIES","MODIFY","MODULE","MONTH","MORE","MOVE","MUMPS","NAME","NAMES","NATIONAL","NATURAL","NEW","NEXT","NO","NOCREATEDB","NOCREATEUSER","NONE","NOT","NOTHING","NOTIFY","NOTNULL","NULL","NULLABLE","NULLIF","NUMBER","NUMERIC","OBJECT","OCTET_LENGTH","OF","OFF","OFFSET","OIDS","OLD","ON","ONLY","OPEN","OPERATION","OPERATOR","OPTION","OPTIONS","ORDER","ORDINALITY","OUT","OUTER","OUTPUT","OVERLAPS","OVERLAY","OVERRIDING","OWNER","PAD","PARAMETER","PARAMETER_MODE","PARAMETER_NAME","PARAMETER_ORDINAL_POSITION","PARAMETER_SPECIFIC_CATALOG","PARAMETER_SPECIFIC_NAME","PARAMETER_SPECIFIC_SCHEMA","PARAMETERS","PARTIAL","PASCAL","PASSWORD","PATH","PENDANT","PLI","POSITION","POSTFIX","PRECISION","PREFIX","PREORDER","PREPARE","PRESERVE","PRIMARY","PRIOR","PRIVILEGES","PROCEDURAL","PROCEDURE","PUBLIC","READ","READS","REAL","RECURSIVE","REF","REFERENCES","REFERENCING","REINDEX","RELATIVE","RENAME","REPEATABLE","REPLACE","RESET","RESTRICT","RESULT","RETURN","RETURNED_LENGTH","RETURNED_OCTET_LENGTH","RETURNED_SQLSTATE","RETURNS","REVOKE","RIGHT","ROLE","ROLLBACK","ROLLUP","ROUTINE","ROUTINE_CATALOG","ROUTINE_NAME","ROUTINE_SCHEMA","ROW","ROW_COUNT","ROWS","RULE","SAVEPOINT","SCALE","SCHEMA","SCHEMA_NAME","SCOPE","SCROLL","SEARCH","SECOND","SECTION","SECURITY","SELECT","SELF","SENSITIVE","SEQUENCE","SERIALIZABLE","SERVER_NAME","SESSION","SESSION_USER","SET","SETOF","SETS","SHARE","SHOW","SIMILAR","SIMPLE","SIZE","SOME","SOURCE","SPACE","SPECIFIC","SPECIFIC_NAME","SPECIFICTYPE","SQL","SQLCODE","SQLERROR","SQLEXCEPTION","SQLSTATE","SQLWARNING","STABLE","START","STATE","STATEMENT","STATIC","STATISTICS","STDIN","STDOUT","STRUCTURE","STYLE","SUBCLASS_ORIGIN","SUBLIST","SUBSTRING","SUM","SYMMETRIC","SYSID","SYSTEM","SYSTEM_USER","TABLE","TABLE_NAME","TEMP","TEMPLATE","TEMPORARY","TERMINATE","THAN","THEN","TIMEZONE_HOUR","TIMEZONE_MINUTE","TO","TOAST","TRAILING","TRANSACTION","TRANSACTION_ACTIVE","TRANSACTIONS_COMMITTED","TRANSACTIONS_ROLLED_BACK","TRANSFORM","TRANSFORMS","TRANSLATE","TRANSLATION","TREAT","TRIGGER","TRIGGER_CATALOG","TRIGGER_NAME","TRIGGER_SCHEMA","TRIM","TRUNCATE","TRUSTED","TYPE","UNCOMMITTED","UNDER","UNENCRYPTED","UNION","UNIQUE","UNKNOWN","UNLISTEN","UNNAMED","UNNEST","UNTIL","UPDATE","UPPER","USAGE","USER","USER_DEFINED_TYPE_CATALOG","USER_DEFINED_TYPE_NAME","USER_DEFINED_TYPE_SCHEMA","USING","VACUUM","VALID","VALUE","VALUES","VARIABLE","VARYING","VERBOSE","VERSION","VIEW","VOLATILE","WHEN","WHENEVER","WHERE","WITH","WITHOUT","WORK","WRITE","YEAR","ZONE","FALSE","TRUE"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["+","-","*","/","||","|/","||/","!","!!","@","&","|","#","<<",">>","%","^","=","!=","<>","<","<=",">",">=","~","~*","!~","!~*","^=",":=","=>","**","..","AND","OR","NOT","##","&&","&<","&>","<->","<^",">^","?#","?-","?-|","@-@","?|","?||","@@","~=","<<=",">>="] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["ABS","CBRT","CEIL","DEGREES","EXP","FLOOR","LN","LOG","MOD","PI","POW","RADIANS","RANDOM","ROUND","SIGN","SQRT","TRUNC","ACOS","ASIN","ATAN","ATAN2","COS","COT","SIN","TAN","BIT_LENGTH","CHAR_LENGTH","CHARACTER_LENGTH","LOWER","OCTET_LENGTH","POSITION","SUBSTRING","TRIM","UPPER","ASCII","BTRIM","CHR","CONVERT","INITCAP","LENGTH","LPAD","LTRIM","PG_CLIENT_ENCODING","REPEAT","RPAD","RTRIM","STRPOS","SUBSTR","TO_ASCII","TRANSLATE","ENCODE","DECODE","TO_CHAR","TO_DATE","TO_TIMESTAMP","TO_NUMBER","AGE","DATE_PART","DATE_TRUNC","EXTRACT","ISFINITE","NOW","TIMEOFDAY","TIMESTAMP","EXTRACT","AREA","BOX","CENTER","DIAMETER","HEIGHT","ISCLOSED","ISOPEN","PCLOSE","NPOINT","POPEN","RADIUS","WIDTH","BOX","CIRCLE","LSEG","PATH","POINT","POLYGON","BROADCAST","HOST","MASKLEN","SET_MASKLEN","NETMASK","NETWORK","ABBREV","NEXTVAL","CURRVAL","SETVAL","COALESCE","NULLIF","HAS_TABLE_PRIVILEGE","PG_GET_VIEWDEF","PG_GET_RULEDEF","PG_GET_INDEXDEF","PG_GET_USERBYID","OBJ_DESCRIPTION","COL_DESCRIPTION","AVG","COUNT","MAX","MIN","STDDEV","SUM","VARIANCE"] >>= withAttribute "Function"))
                        <|>
                        ((pKeyword ["LZTEXT","BIGINT","INT2","INT8","BIGSERIAL","SERIAL8","BIT","BIT VARYING","VARBIT","BOOLEAN","BOOL","BOX","BYTEA","CHARACTER","CHAR","CHARACTER VARYING","VARCHAR","CIDR","CIRCLE","DATE","DOUBLE PRECISION","FLOAT8","INET","INTEGER","INT","INT4","INTERVAL","LINE","LSEG","MACADDR","MONEY","NUMERIC","DECIMAL","OID","PATH","POINT","POLYGON","REAL","SMALLINT","SERIAL","TEXT","TIME","TIMETZ","TIMESTAMP","TIMESTAMPTZ","TIMESTAMP WITH TIMEZONE"] >>= withAttribute "Data Type"))
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
                        ((pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "MultiLineComment")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "rem\\b") >>= withAttribute "Comment") >>~ pushContext "SingleLineComment")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Comment") >>~ pushContext "Identifier")
                        <|>
                        ((pAnyChar ":&" >>= withAttribute "Symbol"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "/$") >>= withAttribute "Symbol"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "@@?[^@ \\t\\r\\n]") >>= withAttribute "Preprocessor") >>~ pushContext "Preprocessor"))
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

parseRules "SingleLineComment" = 
  pzero

parseRules "MultiLineComment" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Identifier" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "Identifier") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Preprocessor" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x
