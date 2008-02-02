{- This module was generated from data in the Kate syntax highlighting file sql.xml, version 1.13,
   by  Yury Lebedev (yurylebedev@mail.ru) -}

module Text.Highlighting.Kate.Syntax.Sql ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "SQL"

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
  setState $ st { synStLanguage = "SQL" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("SQL",["Normal"])], synStLanguage = "SQL", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = False, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t(),%&;?[]{}\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String literal" -> return ()
    "Singleline PL/SQL-style comment" -> (popContext >> return ())
    "Multiline C-style comment" -> return ()
    "SQL*Plus remark directive" -> (popContext >> return ())
    "User-defined identifier" -> (popContext >> return ())
    "SQL*Plus directive to include file" -> (popContext >> return ())
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }

withAttribute attr txt = do
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Operator","Normal"),("Function","Function"),("Data Type","DataType"),("Decimal","DecVal"),("Float","Float"),("String","String"),("String Char","Char"),("Comment","Comment"),("Identifier","Others"),("External Variable","Char"),("Symbol","Char"),("Preprocessor","Others")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String literal","String"),("Singleline PL/SQL-style comment","Comment"),("Multiline C-style comment","Comment"),("SQL*Plus remark directive","Comment"),("User-defined identifier","Identifier"),("SQL*Plus directive to include file","Preprocessor")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword ["ACCESS","ACCOUNT","ADD","ADMIN","ADMINISTER","ADVISE","AFTER","AGENT","ALL","ALL_ROWS","ALLOCATE","ALTER","ANALYZE","ANCILLARY","AND","ANY","ARCHIVE","ARCHIVELOG","AS","ASC","ASSERTION","ASSOCIATE","AT","ATTRIBUTE","ATTRIBUTES","AUDIT","AUTHENTICATED","AUTHID","AUTHORIZATION","AUTOALLOCATE","AUTOEXTEND","AUTOMATIC","BACKUP","BECOME","BEFORE","BEGIN","BEHALF","BETWEEN","BINDING","BITMAP","BLOCK","BLOCK_RANGE","BODY","BOUND","BOTH","BREAK","BROADCAST","BTITLE","BUFFER_POOL","BUILD","BULK","BY","CACHE","CACHE_INSTANCES","CALL","CANCEL","CASCADE","CASE","CATEGORY","CHAINED","CHANGE","CHECK","CHECKPOINT","CHILD","CHOOSE","CHUNK","CLASS","CLEAR","CLONE","CLOSE","CLOSE_CACHED_OPEN_CURSORS","CLUSTER","COALESCE","COLUMN","COLUMNS","COLUMN_VALUE","COMMENT","COMMIT","COMMITTED","COMPATIBILITY","COMPILE","COMPLETE","COMPOSITE_LIMIT","COMPRESS","COMPUTE","CONNECT","CONNECT_TIME","CONSIDER","CONSISTENT","CONSTANT","CONSTRAINT","CONSTRAINTS","CONTAINER","CONTENTS","CONTEXT","CONTINUE","CONTROLFILE","COPY","COST","CPU_PER_CALL","CPU_PER_SESSION","CREATE","CREATE_STORED_OUTLINES","CROSS","CUBE","CURRENT","CURSOR","CYCLE","DANGLING","DATA","DATABASE","DATAFILE","DATAFILES","DBA","DDL","DEALLOCATE","DEBUG","DECLARE","DEFAULT","DEFERRABLE","DEFERRED","DEFINER","DEGREE","DELETE","DEMAND","DESC","DETERMINES","DICTIONARY","DIMENSION","DIRECTORY","DISABLE","DISASSOCIATE","DISCONNECT","DISKGROUP","DISMOUNT","DISTINCT","DISTRIBUTED","DOMAIN","DROP","DYNAMIC","EACH","ELSE","ELSIF","EMPTY","ENABLE","END","ENFORCE","ENTRY","ESCAPE","ESTIMATE","EVENTS","EXCEPT","EXCEPTION","EXCEPTIONS","EXCHANGE","EXCLUDING","EXCLUSIVE","EXEC","EXECUTE","EXISTS","EXPIRE","EXPLAIN","EXPLOSION","EXTENDS","EXTENT","EXTENTS","EXTERNALLY","FAILED_LOGIN_ATTEMPTS","FALSE","FAST","FILE","FILTER","FIRST_ROWS","FLAGGER","FLASHBACK","FLUSH","FOLLOWING","FOR","FORCE","FOREIGN","FREELIST","FREELISTS","FRESH","FROM","FULL","FUNCTION","FUNCTIONS","GENERATED","GLOBAL","GLOBALLY","GLOBAL_NAME","GRANT","GROUP","GROUPS","HASH","HASHKEYS","HAVING","HEADER","HEAP","HIERARCHY","HOUR","ID","IDENTIFIED","IDENTIFIER","IDGENERATORS","IDLE_TIME","IF","IMMEDIATE","IN","INCLUDING","INCREMENT","INCREMENTAL","INDEX","INDEXED","INDEXES","INDEXTYPE","INDEXTYPES","INDICATOR","INITIAL","INITIALIZED","INITIALLY","INITRANS","INNER","INSERT","INSTANCE","INSTANCES","INSTEAD","INTERMEDIATE","INTERSECT","INTO","INVALIDATE","IS","ISOLATION","ISOLATION_LEVEL","JAVA","JOIN","KEEP","KEY","KILL","LABEL","LAYER","LEADING","LEFT","LESS","LEVEL","LIBRARY","LIKE","LIMIT","LINK","LIST","LOCAL","LOCATOR","LOCK","LOCKED","LOGFILE","LOGGING","LOGICAL_READS_PER_CALL","LOGICAL_READS_PER_SESSION","LOGOFF","LOGON","LOOP","MANAGE","MANAGED","MANAGEMENT","MASTER","MATERIALIZED","MAXARCHLOGS","MAXDATAFILES","MAXEXTENTS","MAXINSTANCES","MAXLOGFILES","MAXLOGHISTORY","MAXLOGMEMBERS","MAXSIZE","MAXTRANS","MAXVALUE","METHOD","MEMBER","MERGE","MINIMIZE","MINIMUM","MINEXTENTS","MINUS","MINUTE","MINVALUE","MODE","MODIFY","MONITORING","MOUNT","MOVE","MOVEMENT","MTS_DISPATCHERS","MULTISET","NAMED","NATURAL","NEEDED","NESTED","NESTED_TABLE_ID","NETWORK","NEVER","NEW","NEXT","NLS_CALENDAR","NLS_CHARACTERSET","NLS_COMP","NLS_CURRENCY","NLS_DATE_FORMAT","NLS_DATE_LANGUAGE","NLS_ISO_CURRENCY","NLS_LANG","NLS_LANGUAGE","NLS_NUMERIC_CHARACTERS","NLS_SORT","NLS_SPECIAL_CHARS","NLS_TERRITORY","NO","NOARCHIVELOG","NOAUDIT","NOCACHE","NOCOMPRESS","NOCYCLE","NOFORCE","NOLOGGING","NOMAXVALUE","NOMINIMIZE","NOMINVALUE","NOMONITORING","NONE","NOORDER","NOOVERRIDE","NOPARALLEL","NORELY","NORESETLOGS","NOREVERSE","NORMAL","NOSEGMENT","NOSORT","NOT","NOTHING","NOVALIDATE","NOWAIT","NULL","NULLS","OBJNO","OBJNO_REUSE","OF","OFF","OFFLINE","OID","OIDINDEX","OLD","ON","ONLINE","ONLY","OPCODE","OPEN","OPERATOR","OPTIMAL","OPTIMIZER_GOAL","OPTION","OR","ORDER","ORGANIZATION","OUT","OUTER","OUTLINE","OVER","OVERFLOW","OVERLAPS","OWN","PACKAGE","PACKAGES","PARALLEL","PARAMETERS","PARENT","PARTITION","PARTITIONS","PARTITION_HASH","PARTITION_RANGE","PASSWORD","PASSWORD_GRACE_TIME","PASSWORD_LIFE_TIME","PASSWORD_LOCK_TIME","PASSWORD_REUSE_MAX","PASSWORD_REUSE_TIME","PASSWORD_VERIFY_FUNCTION","PCTFREE","PCTINCREASE","PCTTHRESHOLD","PCTUSED","PCTVERSION","PERCENT","PERMANENT","PLAN","PLSQL_DEBUG","POST_TRANSACTION","PREBUILT","PRECEDING","PREPARE","PRESENT","PRESERVE","PREVIOUS","PRIMARY","PRIOR","PRIVATE","PRIVATE_SGA","PRIVILEGE","PRIVILEGES","PROCEDURE","PROFILE","PUBLIC","PURGE","QUERY","QUEUE","QUOTA","RANDOM","RANGE","RBA","READ","READS","REBUILD","RECORDS_PER_BLOCK","RECOVER","RECOVERABLE","RECOVERY","RECYCLE","REDUCED","REFERENCES","REFERENCING","REFRESH","RELY","RENAME","REPLACE","RESET","RESETLOGS","RESIZE","RESOLVE","RESOLVER","RESOURCE","RESTRICT","RESTRICTED","RESUME","RETURN","RETURNING","REUSE","REVERSE","REVOKE","REWRITE","RIGHT","ROLE","ROLES","ROLLBACK","ROLLUP","ROW","ROWNUM","ROWS","RULE","SAMPLE","SAVEPOINT","SCAN","SCAN_INSTANCES","SCHEMA","SCN","SCOPE","SD_ALL","SD_INHIBIT","SD_SHOW","SEGMENT","SEG_BLOCK","SEG_FILE","SELECT","SELECTIVITY","SEQUENCE","SERIALIZABLE","SERVERERROR","SESSION","SESSION_CACHED_CURSORS","SESSIONS_PER_USER","SET","SHARE","SHARED","SHARED_POOL","SHRINK","SHUTDOWN","SINGLETASK","SIZE","SKIP","SKIP_UNUSABLE_INDEXES","SNAPSHOT","SOME","SORT","SOURCE","SPECIFICATION","SPLIT","SQL_TRACE","STANDBY","START","STARTUP","STATEMENT_ID","STATISTICS","STATIC","STOP","STORAGE","STORE","STRUCTURE","SUBMULTISET","SUBPARTITION","SUBPARTITIONS","SUCCESSFUL","SUMMARY","SUPPLEMENTAL","SUSPEND","SWITCH","SYS_OP_BITVEC","SYS_OP_ENFORCE_NOT_NULL$","SYS_OP_NOEXPAND","SYS_OP_NTCIMG$","SYNONYM","SYSDBA","SYSOPER","SYSTEM","TABLE","TABLES","TABLESPACE","TABLESPACE_NO","TABNO","TEMPFILE","TEMPORARY","THAN","THE","THEN","THREAD","THROUGH","TIMEOUT","TIMEZONE_HOUR","TIMEZONE_MINUTE","TIME_ZONE","TO","TOPLEVEL","TRACE","TRACING","TRAILING","TRANSACTION","TRANSITIONAL","TRIGGER","TRIGGERS","TRUE","TRUNCATE","TYPE","TYPES","UNARCHIVED","UNBOUND","UNBOUNDED","UNDO","UNIFORM","UNION","UNIQUE","UNLIMITED","UNLOCK","UNRECOVERABLE","UNTIL","UNUSABLE","UNUSED","UPD_INDEXES","UPDATABLE","UPDATE","UPPPER","USAGE","USE","USE_STORED_OUTLINES","USER_DEFINED","USING","VALIDATE","VALIDATION","VALUES","VIEW","WHEN","WHENEVER","WHERE","WITH","WITHOUT","WORK","WRITE"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["+","-","*","/","||","=","!=","<>","<","<=",">",">=","~=","^=",":=","=>","**",".."] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["ABS","ACOS","ADD_MONTHS","ASCII","ASCIISTR","ASIN","ATAN","ATAN2","AVG","BFILENAME","BIN_TO_NUM","BITAND","CARDINALITY","CAST","CEIL","CHARTOROWID","CHR","COALESCE","COLLECT","COMPOSE","CONCAT","CONVERT","CORR","CORR_K","CORR_S","COS","COSH","COUNT","COVAR_POP","COVAR_SAMP","CUME_DIST","CURRENT_DATE","CURRENT_TIMESTAMP","CV","DBTIMEZONE","DECODE","DECOMPOSE","DENSE_RANK","DEPTH","DEREF","DUMP","EMPTY_BLOB","EMPTY_CLOB","EXISTSNODE","EXP","EXTRACT","EXTRACTVALUE","FIRST","FIRST_VALUE","FLOOR","FROM_TZ","GREATEST","GROUP_ID","GROUPING","GROUPING_ID","HEXTORAW","INITCAP","INSTR","INSTRB","LAG","LAST","LAST_DAY","LAST_VALUE","LEAD","LEAST","LENGTH","LENGTHB","LN","LNNVL","LOCALTIMESTAMP","LOG","LOWER","LPAD","LTRIM","MAKE_REF","MAX","MEDIAN","MIN","MOD","MONTHS_BETWEEN","NANVL","NCHR","NEW_TIME","NEXT_DAY","NLS_CHARSET_DECL_LEN","NLS_CHARSET_ID","NLS_CHARSET_NAME","NLS_INITCAP","NLS_LOWER","NLS_UPPER","NLSSORT","NTILE","NULLIF","NUMTODSINTERVAL","NUMTOYMINTERVAL","NVL","NVL2","ORA_HASH","ORA_ROWSCN","PERCENT_RANK","PERCENTILE_CONT","PERCENTILE_DISC","POWER","POWERMULTISET","POWERMULTISET_BY_CARDINALITY","PRESENTNNV","PRESENTV","RANK","RATIO_TO_REPORT","RAWTOHEX","RAWTONHEX","REF","REFTOHEX","REGEXP_INSTR","REGEXP_LIKE","REGEXP_REPLACE","REGEXP_SUBSTR","REGR_SLOPE","REGR_INTERCEPT","REGR_COUNT","REGR_R2","REGR_AVGX","REGR_AVGY","REGR_SXX","REGR_SYY","REGR_SXY","REMAINDER","ROUND","ROW_NUMBER","ROWIDTOCHAR","ROWIDTONCHAR","RPAD","RTRIM","SCN_TO_TIMESTAMP","SESSIONTIMEZONE","SIGN","SIN","SINH","SOUNDEX","SQRT","STATS_BINOMIAL_TEST","STATS_CROSSTAB","STATS_F_TEST","STATS_KS_TEST","STATS_MODE","STATS_MW_TEST","STATS_ONE_WAY_ANOVA","STATS_T_TEST_ONE","STATS_T_TEST_PAIRED","STATS_T_TEST_INDEP","STATS_T_TEST_INDEPU","STATS_WSR_TEST","STDDEV","STDDEV_POP","STDDEV_SAMP","SUBSTR","SUBSTRB","SUM","SYS_CONNECT_BY_PATH","SYS_CONTEXT","SYS_DBURIGEN","SYS_EXTRACT_UTC","SYS_GUID","SYS_TYPEID","SYS_XMLAGG","SYS_XMLGEN","SYSDATE","SYSTIMESTAMP","TAN","TANH","TIMESTAMP_TO_SCN","TO_BINARY_DOUBLE","TO_BINARY_FLOAT","TO_CHAR","TO_CLOB","TO_DATE","TO_DSINTERVAL","TO_LOB","TO_MULTI_BYTE","TO_NCHAR","TO_NCLOB","TO_NUMBER","TO_SINGLE_BYTE","TO_TIMESTAMP","TO_TIMESTAMP_TZ","TO_YMINTERVAL","TRANSLATE","TREAT","TRIM","TRUNC","TZ_OFFSET","UID","UNISTR","UPDATEXML","UPPER","USER","USERENV","VALUE","VAR_POP","VAR_SAMP","VARIANCE","VSIZE","WIDTH_BUCKET","XMLAGG","XMLCOLATTVAL","XMLCONCAT","XMLELEMENT","XMLFOREST","XMLSEQUENCE","XMLTRANSFORM"] >>= withAttribute "Function"))
                        <|>
                        ((pKeyword ["ANYDATA","ANYDATASET","ANYTYPE","ARRAY","BFILE","BINARY_DOUBLE","BINARY_FLOAT","BINARY_INTEGER","BLOB","BOOLEAN","CFILE","CHAR","CHARACTER","CLOB","DATE","DAY","DBURITYPE","DEC","DECIMAL","DOUBLE","FLOAT","FLOB","HTTPURITYPE","INT","INTEGER","INTERVAL","LOB","LONG","MLSLABEL","MONTH","NATIONAL","NCHAR","NCLOB","NUMBER","NUMERIC","NVARCHAR","OBJECT","PLS_INTEGER","PRECISION","RAW","RECORD","REAL","ROWID","SECOND","SINGLE","SMALLINT","TIME","TIMESTAMP","URIFACTORYTYPE","URITYPE","UROWID","VARCHAR","VARCHAR2","VARYING","VARRAY","XMLTYPE","YEAR","ZONE"] >>= withAttribute "Data Type"))
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
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "String literal")
                        <|>
                        ((pDetect2Chars False '-' '-' >>= withAttribute "Comment") >>~ pushContext "Singleline PL/SQL-style comment")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Multiline C-style comment")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "rem\\b") >>= withAttribute "Comment") >>~ pushContext "SQL*Plus remark directive")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Identifier") >>~ pushContext "User-defined identifier")
                        <|>
                        ((pRegExpr (compileRegex "(:|&&?)\\w+") >>= withAttribute "External Variable"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "/$") >>= withAttribute "Symbol"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "@@?[^@ \\t\\r\\n]") >>= withAttribute "Preprocessor") >>~ pushContext "SQL*Plus directive to include file"))
     return (attr, result)

parseRules "String literal" = 
  do (attr, result) <- (((pDetect2Chars False '\\' '\'' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pRegExpr (compileRegex "&&?\\w+") >>= withAttribute "External Variable"))
                        <|>
                        ((pDetect2Chars False '\'' '\'' >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Singleline PL/SQL-style comment" = 
  pzero

parseRules "Multiline C-style comment" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "SQL*Plus remark directive" = 
  pzero

parseRules "User-defined identifier" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "Identifier") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "SQL*Plus directive to include file" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x