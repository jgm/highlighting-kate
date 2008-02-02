{- This module was generated from data in the Kate syntax highlighting file erlang.xml, version 1.02,
   by  Bill Ross (bill@emailme.net.au) -}

module Text.Highlighting.Kate.Syntax.Erlang ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Erlang"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.erl"

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
  setState $ st { synStLanguage = "Erlang" }
  context <- currentContext <|> (pushContext "Normal Text" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Erlang",["Normal Text"])], synStLanguage = "Erlang", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal Text" -> (popContext >> return ())
    "isfunction" -> (popContext >> return ())
    "atomquote" -> (popContext >> return ())
    "stringquote" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Pragma","Keyword"),("Function","Function"),("Separator","Function"),("Operator","Keyword"),("Variable","DataType"),("Integer","DecVal"),("Number","BaseN"),("Float","Float"),("Atom","Char"),("String","String"),("Comment","Comment")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("isfunction","Function"),("atomquote","Atom"),("stringquote","String"),("comment","Comment")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "(?:-module|-export|-define|-undef|-ifdef|-ifndef|-else|-endif|-include|-include_lib)") >>= withAttribute "Pragma"))
                        <|>
                        ((pKeyword ["after","begin","case","catch","cond","end","fun","if","let","of","query","receive","all_true","some_true"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["div","rem","or","xor","bor","bxor","bsl","bsr","and","band","not","bnot"] >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "(?:\\+|-|\\*|\\/|==|\\/=|=:=|=\\/=|<|=<|>|>=|\\+\\+|--|=|!|<-)") >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword ["abs","accept","alarm","apply","atom_to_list","binary_to_list","binary_to_term","check_process_code","concat_binary","date","delete_module","disconnect_node","element","erase","exit","float","float_to_list","garbage_collect","get","get_keys","group_leader","halt","hd","integer_to_list","is_alive","is_atom","is_binary","is_boolean","is_float","is_function","is_integer","is_list","is_number","is_pid","is_port","is_process_alive","is_record","is_reference","is_tuple","length","link","list_to_atom","list_to_binary","list_to_float","list_to_integer","list_to_pid","list_to_tuple","load_module","loaded","localtime","make_ref","module_loaded","node","nodes","now","open_port","pid_to_list","port_close","port_command","port_connect","port_control","ports","pre_loaded","process_flag","process_info","processes","purge_module","put","register","registered","round","self","setelement","size","spawn","spawn_link","spawn_opt","split_binary","statistics","term_to_binary","throw","time","tl","trunc","tuple_to_list","unlink","unregister","whereis"] >>= withAttribute "Function"))
                        <|>
                        ((pRegExpr (compileRegex "(?:\\(|\\)|\\{|\\}|\\[|\\]|\\.|\\:|\\||\\|\\||;|\\,|\\?|->|\\#)") >>= withAttribute "Separator"))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '%' >>= withAttribute "Comment") >>~ pushContext "comment")
                        <|>
                        ((pRegExpr (compileRegex "\\b[a-z][_a-z@-Z0-9]*(?:(?=[^_a-z@-Z0-9])|$):\\b[a-z][_a-z@-Z0-9]*(?:(?=[^_a-z@-Z0-9])|$)") >>= withAttribute "Function") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\b[a-z][_a-z@-Z0-9]*(?:(?=[^_a-z@-Z0-9])|$)\\(") >>= withAttribute "Functon") >>~ pushContext "isfunction")
                        <|>
                        ((pRegExpr (compileRegex "\\b[_A-Z][_a-z@-Z0-9]*(?:(?=[^_a-z@-Z0-9])|$)") >>= withAttribute "Variable") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Atom") >>~ pushContext "atomquote")
                        <|>
                        ((pRegExpr (compileRegex "\\b[a-z][_a-z@-Z0-9]*(?:(?=[^_a-z@-Z0-9])|$)") >>= withAttribute "Atom") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "stringquote")
                        <|>
                        ((pRegExpr (compileRegex "[0-9]+\\.[0-9]+(?:[eE][+-]?[0-9]+)?") >>= withAttribute "Float") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\d+#[a-zA-Z0-9]+") >>= withAttribute "Number") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\S") >>= withAttribute "Integer") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[0-9]+") >>= withAttribute "Integer") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "isfunction" = 
  do (attr, result) <- ((pRegExpr (compileRegex "\\b[a-z][_a-z@-Z0-9]*(?:(?=[^_a-z@-Z0-9])|$)") >>= withAttribute "Function") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "atomquote" = 
  do (attr, result) <- ((pRegExpr (compileRegex "(?:(?:\\\\')?[^']*)*'") >>= withAttribute "Atom") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "stringquote" = 
  do (attr, result) <- ((pRegExpr (compileRegex "(?:(?:\\\\\")?[^\"]*)*\"") >>= withAttribute "String") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x