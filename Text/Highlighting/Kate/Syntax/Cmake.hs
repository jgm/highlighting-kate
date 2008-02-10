{- This module was generated from data in the Kate syntax highlighting file cmake.xml, version 1.04,
   by   -}

module Text.Highlighting.Kate.Syntax.Cmake ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "CMake"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "CMakeLists.txt;*.cmake;"

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
  setState $ st { synStLanguage = "CMake" }
  context <- currentContext <|> (pushContext "Normal Text" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("CMake",["Normal Text"])], synStLanguage = "CMake", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal Text" -> return ()
    "Function Args" -> return ()
    "Comment" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Special Args","Others"),("Commands","Keyword"),("Macros","Keyword"),("Variable","DecVal"),("Comment","Comment"),("Region Marker","RegionMarker")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal Text","Normal Text"),("Function Args","Normal Text"),("Comment","Comment")]

parseRules "Normal Text" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["ABSTRACT_FILES","ADD_CUSTOM_COMMAND","ADD_CUSTOM_TARGET","ADD_DEFINITIONS","ADD_DEPENDENCIES","ADD_EXECUTABLE","ADD_LIBRARY","ADD_SUBDIRECTORY","ADD_TEST","AUX_SOURCE_DIRECTORY","BUILD_COMMAND","BUILD_NAME","CMAKE_MINIMUM_REQUIRED","CONFIGURE_FILE","CREATE_TEST_SOURCELIST","ELSE","ELSEIF","ENABLE_TESTING","ENDFOREACH","ENDIF","ENDMACRO","EXEC_PROGRAM","EXPORT_LIBRARY_DEPENDENCIES","FILE","FIND_FILE","FIND_LIBRARY","FIND_PACKAGE","FIND_PATH","FIND_PROGRAM","FLTK_WRAP_UI","FOREACH","GET_CMAKE_PROPERTY","GET_DIRECTORY_PROPERTY","GET_FILENAME_COMPONENT","GET_SOURCE_FILE_PROPERTY","GET_TARGET_PROPERTY","IF","INCLUDE","INCLUDE_DIRECTORIES","INCLUDE_EXTERNAL_MSPROJECT","INCLUDE_REGULAR_EXPRESSION","INSTALL","INSTALL_FILES","INSTALL_PROGRAMS","INSTALL_TARGETS","ITK_WRAP_TCL","LINK_DIRECTORIES","LINK_LIBRARIES","LIST","LOAD_CACHE","LOAD_COMMAND","MACRO","MAKE_DIRECTORY","MARK_AS_ADVANCED","MESSAGE","OPTION","OUTPUT_REQUIRED_FILES","PROJECT","QT_WRAP_CPP","QT_WRAP_UI","REMOVE","REMOVE_DEFINITIONS","SEPARATE_ARGUMENTS","SET","SET_DIRECTORY_PROPERTIES","SET_SOURCE_FILES_PROPERTIES","SET_TARGET_PROPERTIES","SITE_NAME","SOURCE_FILES","SOURCE_FILES_REMOVE","SOURCE_GROUP","STRING","SUBDIRS","SUBDIR_DEPENDS","TARGET_LINK_LIBRARIES","TRY_COMPILE","TRY_RUN","USE_MANGLED_MESA","UTILITY_SOURCE","VARIABLE_REQUIRES","VTK_MAKE_INSTANTIATOR","VTK_WRAP_JAVA","VTK_WRAP_PYTHON","VTK_WRAP_TCL","WRAP_EXCLUDE_FILES","WRITE_FILE"] >>= withAttribute "Commands") >>~ pushContext "Function Args")
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*BEGIN.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#\\s*END.*$") >>= withAttribute "Region Marker"))
                        <|>
                        ((pDetectChar False '#' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{\\s*\\w+\\s*\\}") >>= withAttribute "Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\w+\\s*(?=\\()") >>= withAttribute "Macros")))
     return (attr, result)

parseRules "Function Args" = 
  do (attr, result) <- (((pDetectChar False ')' >>= withAttribute "Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["ABSOLUTE","ABSTRACT","ADDITIONAL_MAKE_CLEAN_FILES","ALL","AND","APPEND","ARCHIVE","ARGS","ASCII","BEFORE","CACHE","CACHE_VARIABLES","CLEAR","CMAKE_FLAGS","CODE","COMMAND","COMMANDS","COMMAND_NAME","COMMENT","COMPARE","COMPILE_FLAGS","COMPONENT","CONFIGURATIONS","COPYONLY","DEFINED","DEFINE_SYMBOL","DEPENDS","DESTINATION","DIRECTORY_PERMISSIONS","DOC","EQUAL","ESCAPE_QUOTES","EXCLUDE","EXCLUDE_FROM_ALL","EXISTS","EXPORT_MACRO","EXT","EXTRA_INCLUDE","FATAL_ERROR","FILE","FILES","FILE_PERMISSIONS","FORCE","FUNCTION","GENERATED","GLOB","GLOB_RECURSE","GREATER","GROUP_SIZE","HEADER_FILE_ONLY","HEADER_LOCATION","IMMEDIATE","INCLUDES","INCLUDE_DIRECTORIES","INCLUDE_INTERNALS","INCLUDE_REGULAR_EXPRESSION","INTERNAL","LESS","LIBRARY","LINK_DIRECTORIES","LINK_FLAGS","LOCATION","MACOSX_BUNDLE","MACROS","MAIN_DEPENDENCY","MAKE_DIRECTORY","MATCH","MATCHALL","MATCHES","MODULE","NAME","NAMES","NAME_WE","NOT","NOTEQUAL","NO_SYSTEM_PATH","OBJECT_DEPENDS","OPTIONAL","OR","OUTPUT","OUTPUT_VARIABLE","PATH","PATHS","PATTERN","PERMISSIONS","POST_BUILD","POST_INSTALL_SCRIPT","PREFIX","PREORDER","PRE_BUILD","PRE_INSTALL_SCRIPT","PRE_LINK","PROGRAM","PROGRAMS","PROGRAM_ARGS","PROPERTIES","QUIET","RANGE","READ","REGEX","REGULAR_EXPRESSION","RENAME","REPLACE","REQUIRED","RETURN_VALUE","RUNTIME","RUNTIME_DIRECTORY","SCRIPT","SEND_ERROR","SHARED","SOURCES","STATIC","STATUS","STREQUAL","STRGREATER","STRLESS","SUFFIX","TARGET","TARGETS","TOLOWER","TOUPPER","USE_SOURCE_PERMISSIONS","VAR","VARIABLES","VERSION","WIN32","WRAP_EXCLUDE","WRITE"] >>= withAttribute "Special Args"))
                        <|>
                        ((pRegExpr (compileRegex "#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\{\\s*\\w+\\s*\\}") >>= withAttribute "Variable")))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
