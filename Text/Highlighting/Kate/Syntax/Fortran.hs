{- This module was generated from data in the Kate syntax highlighting file fortran.xml, version 1.11,
   by  Franchin Matteo (fnch@libero.it) -}

module Text.Highlighting.Kate.Syntax.Fortran ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Fortran"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.f;*.F;*.for;*.FOR;*.f90;*.F90;*.fpp;*.FPP;*.f95;*.F95;"

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
  setState $ st { synStLanguage = "Fortran" }
  context <- currentContext <|> (pushContext "default" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Fortran",["default"])], synStLanguage = "Fortran", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = False, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "default" -> return ()
    "find_preprocessor" -> return ()
    "find_op_and_log" -> return ()
    "find_comments" -> return ()
    "find_symbols" -> return ()
    "inside_func_paren" -> return ()
    "find_io_stmnts" -> return ()
    "find_io_paren" -> return ()
    "format_stmnt" -> return ()
    "find_begin_stmnts" -> return ()
    "find_end_stmnts" -> return ()
    "find_decls" -> return ()
    "find_paren" -> (popContext >> return ())
    "find_intrinsics" -> return ()
    "find_numbers" -> return ()
    "find_strings" -> return ()
    "string_1" -> return ()
    "string_2" -> return ()
    "end_of_string" -> return ()
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Data Type","DataType"),("Decimal","DecVal"),("Float","Float"),("String","String"),("Comment","Comment"),("Symbol","Normal"),("Preprocessor","Others"),("Operator","Keyword"),("Logical","Others"),("IO Function","Function"),("Elemental Procedure","Keyword"),("Inquiry Function","Function"),("Transformational Function","Function"),("Non elemental subroutine","Keyword")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("default","Normal Text"),("find_preprocessor","Normal Text"),("find_op_and_log","Normal Text"),("find_comments","Normal Text"),("find_symbols","Normal Text"),("inside_func_paren","Normal Text"),("find_io_stmnts","Normal Text"),("find_io_paren","Normal Text"),("format_stmnt","Normal Text"),("find_begin_stmnts","Normal Text"),("find_end_stmnts","Normal Text"),("find_decls","Normal Text"),("find_paren","Data Type"),("find_intrinsics","Normal Text"),("find_numbers","Normal Text"),("find_strings","String"),("string_1","String"),("string_2","String"),("end_of_string","String")]

parseRules "default" = 
  do (attr, result) <- (((parseRules "find_strings"))
                        <|>
                        ((parseRules "find_decls"))
                        <|>
                        ((parseRules "find_intrinsics"))
                        <|>
                        ((parseRules "find_io_stmnts"))
                        <|>
                        ((parseRules "find_op_and_log"))
                        <|>
                        ((parseRules "find_numbers"))
                        <|>
                        ((parseRules "find_preprocessor"))
                        <|>
                        ((parseRules "find_comments"))
                        <|>
                        ((parseRules "find_symbols"))
                        <|>
                        ((parseRules "find_end_stmnts"))
                        <|>
                        ((parseRules "find_begin_stmnts")))
     return (attr, result)

parseRules "find_preprocessor" = 
  do (attr, result) <- ((pColumn 0 >> pRegExpr (compileRegex "(#|cDEC\\$|CDEC\\$).*$") >>= withAttribute "Preprocessor"))
     return (attr, result)

parseRules "find_op_and_log" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\.(true|false)\\.") >>= withAttribute "Logical"))
                        <|>
                        ((pRegExpr (compileRegex "\\.[A-Za-z]+\\.") >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "(==|/=|<|<=|>|>=)") >>= withAttribute "Operator")))
     return (attr, result)

parseRules "find_comments" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "[cC\\*].*$") >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "!.*$") >>= withAttribute "Comment")))
     return (attr, result)

parseRules "find_symbols" = 
  do (attr, result) <- (((pDetect2Chars False '*' '*' >>= withAttribute "Keyword"))
                        <|>
                        ((pDetect2Chars False '(' '/' >>= withAttribute "Keyword"))
                        <|>
                        ((pDetect2Chars False '/' ')' >>= withAttribute "Keyword"))
                        <|>
                        ((pAnyChar "&+-*/=?[]^{|}~" >>= withAttribute "Keyword"))
                        <|>
                        ((pAnyChar "()," >>= withAttribute "Symbol")))
     return (attr, result)

parseRules "inside_func_paren" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "inside_func_paren")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "find_strings"))
                        <|>
                        ((parseRules "find_intrinsics"))
                        <|>
                        ((parseRules "find_numbers")))
     return (attr, result)

parseRules "find_io_stmnts" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\b(read|write|backspace|rewind|end\\s*file|close)\\s*[(]") >>= withAttribute "IO Function") >>~ pushContext "find_io_paren")
                        <|>
                        ((pRegExpr (compileRegex "\\bopen\\s*[(]") >>= withAttribute "IO Function") >>~ pushContext "find_io_paren")
                        <|>
                        ((pRegExpr (compileRegex "\\binquire\\s*[(]") >>= withAttribute "IO Function") >>~ pushContext "find_io_paren")
                        <|>
                        ((pRegExpr (compileRegex "\\bformat\\s*[(]") >>= withAttribute "IO Function") >>~ pushContext "format_stmnt")
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s*file\\b") >>= withAttribute "IO Function"))
                        <|>
                        ((pKeyword ["access","backspace","close","inquire","open","print","read","rewind","write","format"] >>= withAttribute "IO Function")))
     return (attr, result)

parseRules "find_io_paren" = 
  do (attr, result) <- (((pDetectChar False '*' >>= withAttribute "IO Function"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Normal Text") >>~ pushContext "inside_func_paren")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "IO Function") >>~ (popContext >> return ()))
                        <|>
                        ((pKeyword ["unit","end","err","fmt","iostat","status","advance","size","eor"] >>= withAttribute "IO Function"))
                        <|>
                        ((pKeyword ["unit","iostat","err","file","exist","opened","number","named","name","access","sequential","direct","form","formatted","unformatted","recl","nextrec","blank","position","action","read","write","readwrite","delim","pad"] >>= withAttribute "IO Function"))
                        <|>
                        ((pKeyword ["unit","iostat","err","file","status","access","form","recl","blank","position","action","delim","pad"] >>= withAttribute "IO Function"))
                        <|>
                        ((parseRules "find_strings"))
                        <|>
                        ((parseRules "find_intrinsics"))
                        <|>
                        ((parseRules "find_numbers"))
                        <|>
                        ((parseRules "find_symbols")))
     return (attr, result)

parseRules "format_stmnt" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "IO Function") >>~ pushContext "format_stmnt")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "IO Function") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[0-9]*/") >>= withAttribute "IO Function"))
                        <|>
                        ((pAnyChar ":" >>= withAttribute "IO Function"))
                        <|>
                        ((parseRules "find_strings"))
                        <|>
                        ((parseRules "find_symbols")))
     return (attr, result)

parseRules "find_begin_stmnts" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\bmodule\\s+procedure\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\b(program|subroutine|function|module|block\\s*data)\\b") >>= withAttribute "Keyword")))
     return (attr, result)

parseRules "find_end_stmnts" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\bend\\s*(program|subroutine|function|module|block\\s*data)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s*(do|if|select|where|forall|interface)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\b") >>= withAttribute "Keyword")))
     return (attr, result)

parseRules "find_decls" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\binteger[\\*]\\d{1,2}") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "\\breal[\\*]\\d{1,2}") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "\\bcomplex[\\*]\\d{1,2}") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\s*type\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pKeyword ["double","precision","parameter","save","pointer","public","private","target","allocatable","optional","sequence"] >>= withAttribute "Data Type"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\s*data\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\s*real\\s*[(]") >>= withAttribute "Data Type") >>~ pushContext "find_paren")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "\\s*real(?![\\w\\*])") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "\\bcharacter[*][0-9]+\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pRegExpr (compileRegex "\\b(type|integer|complex|character|logical|intent|dimension)\\b\\s*[(]") >>= withAttribute "Data Type") >>~ pushContext "find_paren")
                        <|>
                        ((pRegExpr (compileRegex "\\b(type|integer|complex|character|logical|intent|dimension)\\b") >>= withAttribute "Data Type"))
                        <|>
                        ((pDetect2Chars False ':' ':' >>= withAttribute "Data Type")))
     return (attr, result)

parseRules "find_paren" = 
  do (attr, result) <- (((pDetectChar False '(' >>= withAttribute "Data Type") >>~ pushContext "find_paren")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Data Type") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "find_intrinsics" = 
  do (attr, result) <- (((pKeyword ["allocate","break","call","case","common","contains","continue","cycle","deallocate","default","do","forall","where","elsewhere","elseif","else","equivalence","exit","external","for","go","goto","if","implicit","include","interface","intrinsic","namelist","none","nullify","operator","assignment","pause","procedure","pure","elemental","record","recursive","result","return","select","selectcase","stop","then","to","use","only","entry","while"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["abs","cabs","dabs","iabs","aimag","aint","dint","anint","dnint","ceiling","cmplx","dcmplx","dimag","floor","nint","idnint","int","idint","ifix","real","float","sngl","dble","dreal","aprime","dconjg","dfloat","ddmim","rand","modulo","conjg","dprod","dim","ddim","idim","max","amax0","amax1","max0","max1","dmax1","min","amin0","amin1","min0","min1","dmin1","mod","amod","dmod","sign","dsign","isign","acos","dacos","asin","dasin","atan","datan","atan2","datan2","cos","ccos","dcos","cosh","dcosh","exp","cexp","dexp","log","alog","dlog","clog","log10","alog10","dlog10","sin","csin","dsin","sinh","dsinh","sqrt","csqrt","dsqrt","tan","dtan","tanh","dtanh","achar","char","iachar","ichar","lge","lgt","lle","llt","adjustl","adjustr","index","len_trim","scan","verify","logical","exponent","fraction","nearest","rrspacing","scale","set_exponent","spacing","btest","iand","ibclr","ibits","ibset","ieor","ior","ishft","ishftc","not","mvbits","merge"] >>= withAttribute "Elemental Procedure"))
                        <|>
                        ((pKeyword ["associated","present","kind","len","digits","epsilon","huge","maxexponent","minexponent","precision","radix","range","tiny","bit_size","allocated","lbound","ubound","shape","size"] >>= withAttribute "Inquiry Function"))
                        <|>
                        ((pKeyword ["repeat","trim","selected_int_kind","selected_real_kind","transfer","dot_product","matmul","all","any","count","maxval","minval","product","sum","pack","unpack","reshape","spread","cshift","eoshift","transpose","maxloc","minloc"] >>= withAttribute "Transformational Function"))
                        <|>
                        ((pKeyword ["date_and_time","system_clock","random_number","random_seed"] >>= withAttribute "Non elemental subroutine")))
     return (attr, result)

parseRules "find_numbers" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[0-9]*\\.[0-9]+([de][+-]?[0-9]+)?([_]([0-9]+|[a-z][\\w_]*))?") >>= withAttribute "Float"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[0-9]+\\.[0-9]*([de][+-]?[0-9]+)?([_]([0-9]+|[a-z][\\w_]*))?(?![a-z])") >>= withAttribute "Float"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[0-9]+[de][+-]?[0-9]+([_]([0-9]+|[a-z][\\w_]*))?") >>= withAttribute "Float"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[0-9]+([_]([0-9]+|[a-zA-Z][\\w_]*))?") >>= withAttribute "Decimal"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[bozx](['][0-9a-f]+[']|[\"][0-9a-f]+[\"])") >>= withAttribute "Decimal")))
     return (attr, result)

parseRules "find_strings" = 
  do (attr, result) <- (((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "string_1")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "string_2"))
     return (attr, result)

parseRules "string_1" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[^']*'") >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "&\\s*$") >>= withAttribute "Keyword") >>~ pushContext "end_of_string")
                        <|>
                        ((pRegExpr (compileRegex ".*(?=&\\s*$)") >>= withAttribute "String") >>~ pushContext "end_of_string")
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "string_2" = 
  do (attr, result) <- (((pRegExpr (compileRegex "[^\"]*\"") >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "&\\s*$") >>= withAttribute "Keyword") >>~ pushContext "end_of_string")
                        <|>
                        ((pRegExpr (compileRegex ".*(?=&\\s*$)") >>= withAttribute "String") >>~ pushContext "end_of_string")
                        <|>
                        ((popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "end_of_string" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "&\\s*$") >>= withAttribute "Keyword"))
                        <|>
                        ((pFirstNonSpace >> pDetectChar False '&' >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "(!.*)?$") >>= withAttribute "Comment"))
                        <|>
                        ((popContext >> popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x