{- This module was generated from data in the Kate syntax highlighting file rhtml.xml, version 1.00,
   by  Richard Dale rdale@foton.es -}

module Text.Highlighting.Kate.Syntax.Rhtml ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import qualified Text.Highlighting.Kate.Syntax.Css
import qualified Text.Highlighting.Kate.Syntax.Javascript
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Ruby/Rails/RHTML"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.rhtml;*.html.erb"

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
  setState $ st { synStLanguage = "Ruby/Rails/RHTML" }
  context <- currentContext <|> (pushContext "Start" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Ruby/Rails/RHTML",["Start"])], synStLanguage = "Ruby/Rails/RHTML", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStKeywordDelims = " \n\t.():+,-<=>%&*/;[]^{|}~\\", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Start" -> return ()
    "FindHTML" -> return ()
    "FindEntityRefs" -> return ()
    "FindPEntityRefs" -> return ()
    "FindAttributes" -> return ()
    "FindDTDRules" -> return ()
    "Comment" -> return ()
    "CDATA" -> return ()
    "PI" -> return ()
    "Doctype" -> return ()
    "Doctype Internal Subset" -> return ()
    "Doctype Markupdecl" -> return ()
    "Doctype Markupdecl DQ" -> return ()
    "Doctype Markupdecl SQ" -> return ()
    "El Open" -> return ()
    "El Close" -> return ()
    "El Close 2" -> return ()
    "El Close 3" -> return ()
    "CSS" -> return ()
    "CSS content" -> return ()
    "JS" -> return ()
    "JS content" -> return ()
    "JS comment close" -> (popContext >> return ())
    "Value" -> return ()
    "Value NQ" -> (popContext >> popContext >> return ())
    "Value DQ" -> return ()
    "Value SQ" -> return ()
    "rubysourceline" -> (popContext >> return ())
    "rubysource" -> return ()
    "Line Continue" -> (popContext >> return ())
    "Quoted String" -> return ()
    "Apostrophed String" -> return ()
    "Command String" -> return ()
    "Embedded documentation" -> return ()
    "RegEx 1" -> return ()
    "Subst" -> return ()
    "Short Subst" -> (popContext >> return ())
    "Member Access" -> (popContext >> return ())
    "Comment Line" -> (popContext >> return ())
    "General Comment" -> (popContext >> return ())
    "RDoc Label" -> (popContext >> return ())
    "find_heredoc" -> (popContext >> return ())
    "find_indented_heredoc" -> (popContext >> return ())
    "indented_heredoc" -> return ()
    "apostrophed_indented_heredoc" -> return ()
    "normal_heredoc" -> return ()
    "apostrophed_normal_heredoc" -> return ()
    "heredoc_rules" -> return ()
    "find_gdl_input" -> (popContext >> return ())
    "gdl_dq_string_1" -> return ()
    "gdl_dq_string_1_nested" -> return ()
    "gdl_dq_string_2" -> return ()
    "gdl_dq_string_2_nested" -> return ()
    "gdl_dq_string_3" -> return ()
    "gdl_dq_string_3_nested" -> return ()
    "gdl_dq_string_4" -> return ()
    "gdl_dq_string_4_nested" -> return ()
    "gdl_dq_string_5" -> return ()
    "dq_string_rules" -> return ()
    "gdl_token_array_1" -> return ()
    "gdl_token_array_1_nested" -> return ()
    "gdl_token_array_2" -> return ()
    "gdl_token_array_2_nested" -> return ()
    "gdl_token_array_3" -> return ()
    "gdl_token_array_3_nested" -> return ()
    "gdl_token_array_4" -> return ()
    "gdl_token_array_4_nested" -> return ()
    "gdl_token_array_5" -> return ()
    "token_array_rules" -> return ()
    "gdl_apostrophed_1" -> return ()
    "gdl_apostrophed_1_nested" -> return ()
    "gdl_apostrophed_2" -> return ()
    "gdl_apostrophed_2_nested" -> return ()
    "gdl_apostrophed_3" -> return ()
    "gdl_apostrophed_3_nested" -> return ()
    "gdl_apostrophed_4" -> return ()
    "gdl_apostrophed_4_nested" -> return ()
    "gdl_apostrophed_5" -> return ()
    "apostrophed_rules" -> return ()
    "gdl_shell_command_1" -> return ()
    "gdl_shell_command_1_nested" -> return ()
    "gdl_shell_command_2" -> return ()
    "gdl_shell_command_2_nested" -> return ()
    "gdl_shell_command_3" -> return ()
    "gdl_shell_command_3_nested" -> return ()
    "gdl_shell_command_4" -> return ()
    "gdl_shell_command_4_nested" -> return ()
    "gdl_shell_command_5" -> return ()
    "shell_command_rules" -> return ()
    "gdl_regexpr_1" -> return ()
    "gdl_regexpr_1_nested" -> return ()
    "gdl_regexpr_2" -> return ()
    "gdl_regexpr_2_nested" -> return ()
    "gdl_regexpr_3" -> return ()
    "gdl_regexpr_3_nested" -> return ()
    "gdl_regexpr_4" -> return ()
    "gdl_regexpr_4_nested" -> return ()
    "gdl_regexpr_5" -> return ()
    "regexpr_rules" -> return ()
    "DATA" -> return ()
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

styles = [("Ruby Normal Text","Normal"),("Keyword","Keyword"),("Attribute Definition","Others"),("Access Control","Keyword"),("Definition","Keyword"),("Pseudo variable","DecVal"),("Dec","DecVal"),("Float","Float"),("Char","Char"),("Octal","BaseN"),("Hex","BaseN"),("Bin","BaseN"),("Symbol","String"),("String","String"),("Raw String","String"),("Command","String"),("Message","Normal"),("Regular Expression","Others"),("Substitution","Others"),("Data","Normal"),("GDL input","Others"),("Default globals","DataType"),("Global Variable","DataType"),("Global Constant","DataType"),("Constant","DataType"),("Constant Value","DataType"),("Kernel methods","Normal"),("Member","Normal"),("Instance Variable","Others"),("Class Variable","Others"),("Ruby Comment","Comment"),("Blockcomment","Comment"),("Region Marker","Normal"),("RDoc Value","Others"),("Error","Error"),("Alert","Alert"),("Delimiter","Char"),("Expression","Others"),("Operator","Char"),("Normal Text","Normal"),("Comment","Comment"),("CDATA","BaseN"),("Processing Instruction","Keyword"),("Doctype","DataType"),("Element","Keyword"),("Attribute","Others"),("Value","String"),("EntityRef","DecVal"),("PEntityRef","DecVal"),("Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Start","Normal Text"),("FindHTML","Normal Text"),("FindEntityRefs","Normal Text"),("FindPEntityRefs","Normal Text"),("FindAttributes","Normal Text"),("FindDTDRules","Normal Text"),("Comment","Comment"),("CDATA","Normal Text"),("PI","Normal Text"),("Doctype","Normal Text"),("Doctype Internal Subset","Normal Text"),("Doctype Markupdecl","Normal Text"),("Doctype Markupdecl DQ","Value"),("Doctype Markupdecl SQ","Value"),("El Open","Normal Text"),("El Close","Normal Text"),("El Close 2","Normal Text"),("El Close 3","Normal Text"),("CSS","Normal Text"),("CSS content","Normal Text"),("JS","Normal Text"),("JS content","Normal Text"),("JS comment close","Comment"),("Value","Normal Text"),("Value NQ","Normal Text"),("Value DQ","Value"),("Value SQ","Value"),("rubysourceline","RUBY RAILS ERB Text"),("rubysource","RUBY RAILS ERB Text"),("Line Continue","Ruby Normal Text"),("Quoted String","String"),("Apostrophed String","Raw String"),("Command String","Command"),("Embedded documentation","Ruby Comment"),("RegEx 1","Regular Expression"),("Subst","Ruby Normal Text"),("Short Subst","Substitution"),("Member Access","Member"),("Comment Line","Ruby Comment"),("General Comment","Ruby Comment"),("RDoc Label","RDoc Value"),("find_heredoc","Ruby Normal Text"),("find_indented_heredoc","Ruby Normal Text"),("indented_heredoc","Ruby Normal Text"),("apostrophed_indented_heredoc","Ruby Normal Text"),("normal_heredoc","Ruby Normal Text"),("apostrophed_normal_heredoc","Ruby Normal Text"),("heredoc_rules","Ruby Normal Text"),("find_gdl_input","Ruby Normal Text"),("gdl_dq_string_1","String"),("gdl_dq_string_1_nested","String"),("gdl_dq_string_2","String"),("gdl_dq_string_2_nested","String"),("gdl_dq_string_3","String"),("gdl_dq_string_3_nested","String"),("gdl_dq_string_4","String"),("gdl_dq_string_4_nested","String"),("gdl_dq_string_5","String"),("dq_string_rules","String"),("gdl_token_array_1","String"),("gdl_token_array_1_nested","String"),("gdl_token_array_2","String"),("gdl_token_array_2_nested","String"),("gdl_token_array_3","String"),("gdl_token_array_3_nested","String"),("gdl_token_array_4","String"),("gdl_token_array_4_nested","String"),("gdl_token_array_5","String"),("token_array_rules","String"),("gdl_apostrophed_1","Raw String"),("gdl_apostrophed_1_nested","Raw String"),("gdl_apostrophed_2","Raw String"),("gdl_apostrophed_2_nested","Raw String"),("gdl_apostrophed_3","Raw String"),("gdl_apostrophed_3_nested","Raw String"),("gdl_apostrophed_4","Raw String"),("gdl_apostrophed_4_nested","Raw String"),("gdl_apostrophed_5","Raw String"),("apostrophed_rules","Raw String"),("gdl_shell_command_1","Command"),("gdl_shell_command_1_nested","Command"),("gdl_shell_command_2","Command"),("gdl_shell_command_2_nested","Command"),("gdl_shell_command_3","Command"),("gdl_shell_command_3_nested","Command"),("gdl_shell_command_4","Command"),("gdl_shell_command_4_nested","Command"),("gdl_shell_command_5","Command"),("shell_command_rules","Command"),("gdl_regexpr_1","Regular Expression"),("gdl_regexpr_1_nested","Regular Expression"),("gdl_regexpr_2","Regular Expression"),("gdl_regexpr_2_nested","Regular Expression"),("gdl_regexpr_3","Regular Expression"),("gdl_regexpr_3_nested","Regular Expression"),("gdl_regexpr_4","Regular Expression"),("gdl_regexpr_4_nested","Regular Expression"),("gdl_regexpr_5","Regular Expression"),("regexpr_rules","Regular Expression"),("DATA","Data")]

parseRules "Start" = 
  do (attr, result) <- ((parseRules "FindHTML"))
     return (attr, result)

parseRules "FindHTML" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pString False "%" >>= withAttribute "Keyword") >>~ pushContext "rubysourceline")
                        <|>
                        ((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pString False "<![CDATA[" >>= withAttribute "CDATA") >>~ pushContext "CDATA")
                        <|>
                        ((pRegExpr (compileRegex "<!DOCTYPE\\s+") >>= withAttribute "Doctype") >>~ pushContext "Doctype")
                        <|>
                        ((pRegExpr (compileRegex "<\\?[\\w:-]*") >>= withAttribute "Processing Instruction") >>~ pushContext "PI")
                        <|>
                        ((pRegExpr (compileRegex "<style\\b") >>= withAttribute "Element") >>~ pushContext "CSS")
                        <|>
                        ((pRegExpr (compileRegex "<script\\b") >>= withAttribute "Element") >>~ pushContext "JS")
                        <|>
                        ((pRegExpr (compileRegex "<pre\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<div\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<table\\b") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "<[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Element") >>~ pushContext "El Open")
                        <|>
                        ((pRegExpr (compileRegex "</pre\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</div\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</table\\b") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((pRegExpr (compileRegex "</[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Element") >>~ pushContext "El Close")
                        <|>
                        ((parseRules "FindDTDRules"))
                        <|>
                        ((parseRules "FindEntityRefs")))
     return (attr, result)

parseRules "FindEntityRefs" = 
  do (attr, result) <- (((pRegExpr (compileRegex "&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z_:][\\w.:_-]*);") >>= withAttribute "EntityRef"))
                        <|>
                        ((pAnyChar "&<" >>= withAttribute "Error")))
     return (attr, result)

parseRules "FindPEntityRefs" = 
  do (attr, result) <- (((pRegExpr (compileRegex "&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z_:][\\w.:_-]*);") >>= withAttribute "EntityRef"))
                        <|>
                        ((pRegExpr (compileRegex "%[A-Za-z_:][\\w.:_-]*;") >>= withAttribute "PEntityRef"))
                        <|>
                        ((pAnyChar "&%" >>= withAttribute "Error")))
     return (attr, result)

parseRules "FindAttributes" = 
  do (attr, result) <- (((pColumn 0 >> pRegExpr (compileRegex "[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Attribute"))
                        <|>
                        ((pRegExpr (compileRegex "\\s+[A-Za-z_:][\\w.:_-]*") >>= withAttribute "Attribute"))
                        <|>
                        ((pDetectChar False '=' >>= withAttribute "Attribute") >>~ pushContext "Value"))
     return (attr, result)

parseRules "FindDTDRules" = 
  do (attr, result) <- ((pRegExpr (compileRegex "<!(ELEMENT|ENTITY|ATTLIST|NOTATION)\\b") >>= withAttribute "Doctype") >>~ pushContext "Doctype Markupdecl")
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment"))
                        <|>
                        ((pString False "-->" >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "-(-(?!->))+") >>= withAttribute "Error")))
     return (attr, result)

parseRules "CDATA" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pString False "]]>" >>= withAttribute "CDATA") >>~ (popContext >> return ()))
                        <|>
                        ((pString False "]]&gt;" >>= withAttribute "EntityRef")))
     return (attr, result)

parseRules "PI" = 
  do (attr, result) <- ((pDetect2Chars False '?' '>' >>= withAttribute "Processing Instruction") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Doctype" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Doctype") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Doctype") >>~ pushContext "Doctype Internal Subset"))
     return (attr, result)

parseRules "Doctype Internal Subset" = 
  do (attr, result) <- (((pDetectChar False ']' >>= withAttribute "Doctype") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindDTDRules"))
                        <|>
                        ((pString False "<!--" >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pRegExpr (compileRegex "<\\?[\\w:-]*") >>= withAttribute "Processing Instruction") >>~ pushContext "PI")
                        <|>
                        ((parseRules "FindPEntityRefs")))
     return (attr, result)

parseRules "Doctype Markupdecl" = 
  do (attr, result) <- (((pDetectChar False '>' >>= withAttribute "Doctype") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Value") >>~ pushContext "Doctype Markupdecl DQ")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Value") >>~ pushContext "Doctype Markupdecl SQ"))
     return (attr, result)

parseRules "Doctype Markupdecl DQ" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Value") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindPEntityRefs")))
     return (attr, result)

parseRules "Doctype Markupdecl SQ" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Value") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindPEntityRefs")))
     return (attr, result)

parseRules "El Open" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetect2Chars False '/' '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "FindAttributes"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "El Close" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "El Close 2" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "El Close 3" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ (popContext >> popContext >> popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "CSS" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetect2Chars False '/' '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ pushContext "CSS content")
                        <|>
                        ((parseRules "FindAttributes"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "CSS content" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pRegExpr (compileRegex "</style\\b") >>= withAttribute "Element") >>~ pushContext "El Close 2")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Css.parseExpression)))
     return (attr, result)

parseRules "JS" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetect2Chars False '/' '>' >>= withAttribute "Element") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Element") >>~ pushContext "JS content")
                        <|>
                        ((parseRules "FindAttributes"))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Error")))
     return (attr, result)

parseRules "JS content" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pRegExpr (compileRegex "</script\\b") >>= withAttribute "Element") >>~ pushContext "El Close 2")
                        <|>
                        ((pRegExpr (compileRegex "//(?=.*</script\\b)") >>= withAttribute "Comment") >>~ pushContext "JS comment close")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Javascript.parseExpression)))
     return (attr, result)

parseRules "JS comment close" = 
  do (attr, result) <- (((pRegExpr (compileRegex "</script\\b") >>= withAttribute "Element") >>~ pushContext "El Close 3")
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd))))
     return (attr, result)

parseRules "Value" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Value") >>~ pushContext "Value DQ")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Value") >>~ pushContext "Value SQ")
                        <|>
                        ((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        (pushContext "Value NQ" >> return ([], "")))
     return (attr, result)

parseRules "Value NQ" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((parseRules "FindEntityRefs"))
                        <|>
                        ((pRegExpr (compileRegex "/(?!>)") >>= withAttribute "Value"))
                        <|>
                        ((pRegExpr (compileRegex "[^/><\"'\\s]") >>= withAttribute "Value"))
                        <|>
                        ((popContext >> popContext >> return ()) >> return ([], "")))
     return (attr, result)

parseRules "Value DQ" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Value") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindEntityRefs")))
     return (attr, result)

parseRules "Value SQ" = 
  do (attr, result) <- (((pRegExpr (compileRegex "<%=?") >>= withAttribute "Keyword") >>~ pushContext "rubysource")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Value") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "FindEntityRefs")))
     return (attr, result)

parseRules "rubysourceline" = 
  do (attr, result) <- ((parseRules "rubysource"))
     return (attr, result)

parseRules "rubysource" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "Ruby Normal Text") >>~ pushContext "Line Continue")
                        <|>
                        ((pRegExpr (compileRegex "-?%>") >>= withAttribute "Keyword") >>~ (popContext >> return ()))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "__END__$") >>= withAttribute "Keyword") >>~ pushContext "DATA")
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "#!\\/.*") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "(\\=|\\(|\\[|\\{)\\s*(if|unless|while|until)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "(while|until)\\b(?!.*\\bdo\\b)") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\;\\s*(while|until)\\b(?!.*\\bdo\\b)") >>= withAttribute "Keyword"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "(if|unless)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\;\\s*(if|unless)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bclass\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bmodule\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bbegin\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bfor\\b(?!.*\\bdo\\b)") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bcase\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bdo\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bdef\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "\\bend\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((pRegExpr (compileRegex "(\\b|^\\s*)(else|elsif|rescue|ensure)(\\s+|$)") >>= withAttribute "Keyword"))
                        <|>
                        ((pString False "..." >>= withAttribute "Operator"))
                        <|>
                        ((pDetect2Chars False '.' '.' >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\.[_a-z][_a-zA-Z0-9]*(\\?|\\!|\\b)") >>= withAttribute "Message"))
                        <|>
                        ((pRegExpr (compileRegex "\\s\\?(\\\\M\\-)?(\\\\C\\-)?\\\\?\\S") >>= withAttribute "Dec"))
                        <|>
                        ((pKeyword ["BEGIN","END","and","begin","break","case","defined?","do","else","elsif","end","ensure","for","if","in","include","next","not","or","redo","rescue","retry","return","then","unless","until","when","while","yield"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword ["attr_reader","attr_writer","attr_accessor"] >>= withAttribute "Attribute Definition"))
                        <|>
                        ((pKeyword ["private_class_method","private","protected","public_class_method","public"] >>= withAttribute "Access Control"))
                        <|>
                        ((pKeyword ["alias","module","class","def","undef"] >>= withAttribute "Definition"))
                        <|>
                        ((pKeyword ["self","super","nil","false","true","caller","__FILE__","__LINE__"] >>= withAttribute "Pseudo variable"))
                        <|>
                        ((pKeyword ["$stdout","$defout","$stderr","$deferr","$stdin"] >>= withAttribute "Default globals"))
                        <|>
                        ((pKeyword ["abort","at_exit","autoload","autoload?","binding","block_given?","callcc","caller","catch","chomp","chomp!","chop","chop!","eval","exec","exit","exit!","fail","fork","format","getc","gets","global_variables","gsub","gsub!","iterator?","lambda","load","local_variables","loop","method_missing","open","p","print","printf","proc","putc","puts","raise","rand","readline","readlines","require","scan","select","set_trace_func","sleep","split","sprintf","srand","sub","sub!","syscall","system","test","throw","trace_var","trap","untrace_var","warn","auto_complete_field","auto_complete_result","auto_discovery_link_tag","auto_link","benchmark","button_to","cache","capture","check_box","check_box_tag","collection_select","concat","content_for","content_tag","country_options_for_select","country_select","current_page?","date_select","datetime_select","debug","define_javascript_functions","distance_of_time_in_words","distance_of_time_in_words_to_now","draggable_element","drop_receiving_element","end_form_tag","error_message_on","error_messages_for","escape_javascript","evaluate_remote_response","excerpt","file_field","file_field_tag","finish_upload_status","form","form_remote_tag","form_tag","form_tag_with_upload_progress","h","hidden_field","hidden_field_tag","highlight","human_size","image_path","image_submit_tag","image_tag","input","javascript_include_tag","javascript_path","javascript_tag","link_image_to","link_to","link_to_function","link_to_if","link_to_image","link_to_remote","link_to_unless","link_to_unless_current","mail_to","markdown","number_to_currency","number_to_human_size","number_to_percentage","number_to_phone","number_with_delimiter","number_with_precision","observe_field","observe_form","option_groups_from_collection_for_select","options_for_select","options_from_collection_for_select","pagination_links","password_field","password_field_tag","periodically_call_remote","pluralize","radio_button","radio_button_tag","register_template_handler","render","render_file","render_template","sanitize","select","select_date","select_datetime","select_day","select_hour","select_minute","select_month","select_second","select_tag","select_time","select_year","simple_format","sortable_element","start_form_tag","strip_links","stylesheet_link_tag","stylesheet_path","submit_tag","submit_to_remote","tag","text_area","text_area_tag","text_field","text_field_tag","text_field_with_auto_complete","textilize","textilize_without_paragraph","time_ago_in_words","time_zone_options_for_select","time_zone_select","truncate","update_element_function","upload_progress_status","upload_progress_text","upload_progress_update_bar_js","upload_status_progress_bar_tag","upload_status_tag","upload_status_text_tag","url_for","visual_effect","word_wrap"] >>= withAttribute "Kernel methods"))
                        <|>
                        ((pRegExpr (compileRegex "\\$[a-zA-Z_0-9]+") >>= withAttribute "Global Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$\\-[a-zA-z_]\\b") >>= withAttribute "Global Variable"))
                        <|>
                        ((pRegExpr (compileRegex "\\$[\\d_*`\\!:?'/\\\\\\-\\&]") >>= withAttribute "Default globals"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[_A-Z]+[A-Z_0-9]+\\b") >>= withAttribute "Global Constant"))
                        <|>
                        ((pRegExpr (compileRegex "\\b[A-Z]+_*([0-9]|[a-z])[_a-zA-Z0-9]*\\b") >>= withAttribute "Constant"))
                        <|>
                        ((pRegExpr (compileRegex "\\b\\-?0[xX][_0-9a-fA-F]+") >>= withAttribute "Hex"))
                        <|>
                        ((pRegExpr (compileRegex "\\b\\-?0[bB][_01]+") >>= withAttribute "Bin"))
                        <|>
                        ((pRegExpr (compileRegex "\\b\\-?0[1-7][_0-7]*") >>= withAttribute "Octal"))
                        <|>
                        ((pRegExpr (compileRegex "\\b\\-?[0-9][0-9_]*\\.[0-9][0-9_]*([eE]\\-?[1-9][0-9]*(\\.[0-9]*)?)?") >>= withAttribute "Float"))
                        <|>
                        ((pRegExpr (compileRegex "\\b\\-?[1-9][0-9_]*\\b") >>= withAttribute "Dec"))
                        <|>
                        ((pInt >>= withAttribute "Dec"))
                        <|>
                        ((pHlCChar >>= withAttribute "Char"))
                        <|>
                        ((pColumn 0 >> pString False "=begin" >>= withAttribute "Blockcomment") >>~ pushContext "Embedded documentation")
                        <|>
                        ((pRegExpr (compileRegex "\\s*<<-(?=\\w+|[\"'])") >>= withAttribute "Operator") >>~ pushContext "find_indented_heredoc")
                        <|>
                        ((pRegExpr (compileRegex "\\s*<<(?=\\w+|[\"'])") >>= withAttribute "Operator") >>~ pushContext "find_heredoc")
                        <|>
                        ((pDetectChar False '.' >>= withAttribute "Operator"))
                        <|>
                        ((pDetect2Chars False '&' '&' >>= withAttribute "Operator"))
                        <|>
                        ((pDetect2Chars False '|' '|' >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\s[\\?\\:\\%/]\\s") >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "[|&<>\\^\\+*~\\-=]+") >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "\\s!") >>= withAttribute "Operator"))
                        <|>
                        ((pRegExpr (compileRegex "/=\\s") >>= withAttribute "Operator"))
                        <|>
                        ((pString False "%=" >>= withAttribute "Operator"))
                        <|>
                        ((pDetect2Chars False ':' ':' >>= withAttribute "Operator") >>~ pushContext "Member Access")
                        <|>
                        ((pRegExpr (compileRegex ":[a-zA-Z_][a-zA-Z0-9_]*") >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "Quoted String")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Raw String") >>~ pushContext "Apostrophed String")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Command") >>~ pushContext "Command String")
                        <|>
                        ((pString False "?#" >>= withAttribute "Normal Text"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "#\\s*BEGIN.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pColumn 0 >> pRegExpr (compileRegex "#\\s*END.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "#") >>= withAttribute "Comment") >>~ pushContext "Comment Line")
                        <|>
                        ((pRegExpr (compileRegex "\\s#") >>= withAttribute "Comment") >>~ pushContext "General Comment")
                        <|>
                        ((pRegExpr (compileRegex "[\\[\\]]+") >>= withAttribute "Delimiter"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Delimiter"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Delimiter"))
                        <|>
                        ((pRegExpr (compileRegex "@[a-zA-Z_0-9]+") >>= withAttribute "Instance Variable"))
                        <|>
                        ((pRegExpr (compileRegex "@@[a-zA-Z_0-9]+") >>= withAttribute "Class Variable"))
                        <|>
                        ((pDetectChar False '/' >>= withAttribute "Regular Expression") >>~ pushContext "RegEx 1")
                        <|>
                        ((pRegExpr (compileRegex "\\s*[%](?=[Qqxw]?[^\\s>])") >>= withAttribute "GDL input") >>~ pushContext "find_gdl_input"))
     return (attr, result)

parseRules "Line Continue" = 
  do (attr, result) <- (((pFirstNonSpace >> pRegExpr (compileRegex "(while|until)\\b(?!.*\\bdo\\b)") >>= withAttribute "Keyword"))
                        <|>
                        ((pFirstNonSpace >> pRegExpr (compileRegex "(if|unless)\\b") >>= withAttribute "Keyword"))
                        <|>
                        ((parseRules "rubysource")))
     return (attr, result)

parseRules "Quoted String" = 
  do (attr, result) <- (((pString False "\\\\" >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\\\\"") >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution") >>~ pushContext "Short Subst")
                        <|>
                        ((pDetect2Chars False '#' '{' >>= withAttribute "Substitution") >>~ pushContext "Subst")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Apostrophed String" = 
  do (attr, result) <- (((pString False "\\\\" >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\\\'") >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Raw String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Command String" = 
  do (attr, result) <- (((pString False "\\\\" >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\\\`") >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution") >>~ pushContext "Short Subst")
                        <|>
                        ((pDetect2Chars False '#' '{' >>= withAttribute "Substitution") >>~ pushContext "Subst")
                        <|>
                        ((pHlCChar >>= withAttribute "Char") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Command") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Embedded documentation" = 
  do (attr, result) <- ((pColumn 0 >> pString False "=end" >>= withAttribute "Ruby Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "RegEx 1" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\\\/") >>= withAttribute "Regular Expression"))
                        <|>
                        ((pRegExpr (compileRegex "[^\\\\]$") >>= withAttribute "Regular Expression") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution") >>~ pushContext "Short Subst")
                        <|>
                        ((pDetect2Chars False '#' '{' >>= withAttribute "Substitution") >>~ pushContext "Subst")
                        <|>
                        ((pRegExpr (compileRegex "/[uiomxn]*") >>= withAttribute "Regular Expression") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Subst" = 
  do (attr, result) <- (((pDetectChar False '}' >>= withAttribute "Substitution") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "rubysource")))
     return (attr, result)

parseRules "Short Subst" = 
  do (attr, result) <- (((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution"))
                        <|>
                        ((pRegExpr (compileRegex "\\w(?!\\w)") >>= withAttribute "Substitution") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Member Access" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\.?[_a-z]\\w*(\\?|\\!)?(?=[^\\w\\d\\.\\:])") >>= withAttribute "Message") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\.?[_a-z]\\w*(\\?|\\!)?") >>= withAttribute "Message"))
                        <|>
                        ((pRegExpr (compileRegex "[A-Z]+_*(\\d|[a-z])\\w*(?=[^\\w\\d\\.\\:])") >>= withAttribute "Constant") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[A-Z]+_*([0-9]|[a-z])\\w*") >>= withAttribute "Constant"))
                        <|>
                        ((pRegExpr (compileRegex "[_A-Z][_A-Z0-9]*(?=[^\\w\\d\\.\\:])") >>= withAttribute "Constant Value") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[_A-Z][_A-Z0-9]*") >>= withAttribute "Constant Value"))
                        <|>
                        ((pDetect2Chars False ':' ':' >>= withAttribute "Operator"))
                        <|>
                        ((pDetectChar False '.' >>= withAttribute "Member"))
                        <|>
                        ((pAnyChar "=+-*/%|&[]{}~" >>= withAttribute "Operator") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '#' >>= withAttribute "Ruby Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pAnyChar "()\\" >>= withAttribute "Ruby Normal Text") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\W") >>= withAttribute "Member") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Comment Line" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\w\\:\\:\\s") >>= withAttribute "Ruby Comment") >>~ pushContext "RDoc Label")
                        <|>
                        ((pKeyword ["TODO","FIXME","NOTE"] >>= withAttribute "Alert"))
                        <|>
                        ((pRegExpr (compileRegex "-?%>") >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "General Comment" = 
  do (attr, result) <- ((pKeyword ["TODO","FIXME","NOTE"] >>= withAttribute "Dec"))
     return (attr, result)

parseRules "RDoc Label" = 
  pzero

parseRules "find_heredoc" = 
  do (attr, result) <- (((pRegExpr (compileRegex "'(\\w+)'") >>= withAttribute "Keyword") >>~ pushContext "apostrophed_normal_heredoc")
                        <|>
                        ((pRegExpr (compileRegex "\"?(\\w+)\"?") >>= withAttribute "Keyword") >>~ pushContext "normal_heredoc"))
     return (attr, result)

parseRules "find_indented_heredoc" = 
  do (attr, result) <- (((pRegExpr (compileRegex "'(\\w+)'") >>= withAttribute "Keyword") >>~ pushContext "apostrophed_indented_heredoc")
                        <|>
                        ((pRegExpr (compileRegex "\"?(\\w+)\"?") >>= withAttribute "Keyword") >>~ pushContext "indented_heredoc"))
     return (attr, result)

parseRules "indented_heredoc" = 
  do (attr, result) <- (((pFirstNonSpace >> pRegExprDynamic "%1$" >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "heredoc_rules")))
     return (attr, result)

parseRules "apostrophed_indented_heredoc" = 
  do (attr, result) <- ((pFirstNonSpace >> pRegExprDynamic "%1$" >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules "normal_heredoc" = 
  do (attr, result) <- (((pColumn 0 >> pRegExprDynamic "%1$" >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "heredoc_rules")))
     return (attr, result)

parseRules "apostrophed_normal_heredoc" = 
  do (attr, result) <- ((pColumn 0 >> pRegExprDynamic "%1$" >>= withAttribute "Keyword") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules "heredoc_rules" = 
  do (attr, result) <- (((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution") >>~ pushContext "Short Subst")
                        <|>
                        ((pDetect2Chars False '#' '{' >>= withAttribute "Substitution") >>~ pushContext "Subst"))
     return (attr, result)

parseRules "find_gdl_input" = 
  do (attr, result) <- (((pRegExpr (compileRegex "w\\(") >>= withAttribute "GDL input") >>~ pushContext "gdl_token_array_1")
                        <|>
                        ((pRegExpr (compileRegex "w\\{") >>= withAttribute "GDL input") >>~ pushContext "gdl_token_array_2")
                        <|>
                        ((pRegExpr (compileRegex "w\\[") >>= withAttribute "GDL input") >>~ pushContext "gdl_token_array_3")
                        <|>
                        ((pRegExpr (compileRegex "w<") >>= withAttribute "GDL input") >>~ pushContext "gdl_token_array_4")
                        <|>
                        ((pRegExpr (compileRegex "w([^\\s\\w])") >>= withAttribute "GDL input") >>~ pushContext "gdl_token_array_5")
                        <|>
                        ((pRegExpr (compileRegex "q\\(") >>= withAttribute "GDL input") >>~ pushContext "gdl_apostrophed_1")
                        <|>
                        ((pRegExpr (compileRegex "q\\{") >>= withAttribute "GDL input") >>~ pushContext "gdl_apostrophed_2")
                        <|>
                        ((pRegExpr (compileRegex "q\\[") >>= withAttribute "GDL input") >>~ pushContext "gdl_apostrophed_3")
                        <|>
                        ((pRegExpr (compileRegex "q<") >>= withAttribute "GDL input") >>~ pushContext "gdl_apostrophed_4")
                        <|>
                        ((pRegExpr (compileRegex "q([^\\s\\w])") >>= withAttribute "GDL input") >>~ pushContext "gdl_apostrophed_5")
                        <|>
                        ((pRegExpr (compileRegex "x\\(") >>= withAttribute "GDL input") >>~ pushContext "gdl_shell_command_1")
                        <|>
                        ((pRegExpr (compileRegex "x\\{") >>= withAttribute "GDL input") >>~ pushContext "gdl_shell_command_2")
                        <|>
                        ((pRegExpr (compileRegex "x\\[") >>= withAttribute "GDL input") >>~ pushContext "gdl_shell_command_3")
                        <|>
                        ((pRegExpr (compileRegex "x<") >>= withAttribute "GDL input") >>~ pushContext "gdl_shell_command_4")
                        <|>
                        ((pRegExpr (compileRegex "x([^\\s\\w])") >>= withAttribute "GDL input") >>~ pushContext "gdl_shell_command_5")
                        <|>
                        ((pRegExpr (compileRegex "r\\(") >>= withAttribute "GDL input") >>~ pushContext "gdl_regexpr_1")
                        <|>
                        ((pRegExpr (compileRegex "r\\{") >>= withAttribute "GDL input") >>~ pushContext "gdl_regexpr_2")
                        <|>
                        ((pRegExpr (compileRegex "r\\[") >>= withAttribute "GDL input") >>~ pushContext "gdl_regexpr_3")
                        <|>
                        ((pRegExpr (compileRegex "r<") >>= withAttribute "GDL input") >>~ pushContext "gdl_regexpr_4")
                        <|>
                        ((pRegExpr (compileRegex "r([^\\s\\w])") >>= withAttribute "GDL input") >>~ pushContext "gdl_regexpr_5")
                        <|>
                        ((pRegExpr (compileRegex "Q?\\(") >>= withAttribute "GDL input") >>~ pushContext "gdl_dq_string_1")
                        <|>
                        ((pRegExpr (compileRegex "Q?\\{") >>= withAttribute "GDL input") >>~ pushContext "gdl_dq_string_2")
                        <|>
                        ((pRegExpr (compileRegex "Q?\\[") >>= withAttribute "GDL input") >>~ pushContext "gdl_dq_string_3")
                        <|>
                        ((pRegExpr (compileRegex "Q?<") >>= withAttribute "GDL input") >>~ pushContext "gdl_dq_string_4")
                        <|>
                        ((pRegExpr (compileRegex "Q?([^\\s\\w])") >>= withAttribute "GDL input") >>~ pushContext "gdl_dq_string_5"))
     return (attr, result)

parseRules "gdl_dq_string_1" = 
  do (attr, result) <- (((parseRules "dq_string_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_dq_string_1_nested" = 
  do (attr, result) <- (((parseRules "dq_string_rules"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_dq_string_2" = 
  do (attr, result) <- (((parseRules "dq_string_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '}' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_2_nested"))
     return (attr, result)

parseRules "gdl_dq_string_2_nested" = 
  do (attr, result) <- (((pDetectChar False '{' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_2_nested")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "dq_string_rules")))
     return (attr, result)

parseRules "gdl_dq_string_3" = 
  do (attr, result) <- (((parseRules "dq_string_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_dq_string_3_nested" = 
  do (attr, result) <- (((pDetectChar False '[' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "dq_string_rules")))
     return (attr, result)

parseRules "gdl_dq_string_4" = 
  do (attr, result) <- (((parseRules "dq_string_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '>' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_dq_string_4_nested" = 
  do (attr, result) <- (((pDetectChar False '<' >>= withAttribute "String") >>~ pushContext "gdl_dq_string_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "dq_string_rules")))
     return (attr, result)

parseRules "gdl_dq_string_5" = 
  do (attr, result) <- (((parseRules "dq_string_rules"))
                        <|>
                        ((pRegExprDynamic "\\\\%1" >>= withAttribute "String"))
                        <|>
                        ((pRegExprDynamic "\\s*%1" >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "dq_string_rules" = 
  do (attr, result) <- (((pDetect2Chars False '\\' '\\' >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution") >>~ pushContext "Short Subst")
                        <|>
                        ((pDetect2Chars False '#' '{' >>= withAttribute "Substitution") >>~ pushContext "Subst"))
     return (attr, result)

parseRules "gdl_token_array_1" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "String") >>~ pushContext "gdl_token_array_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_token_array_1_nested" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "String") >>~ pushContext "gdl_token_array_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_token_array_2" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '}' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "String") >>~ pushContext "gdl_token_array_2_nested"))
     return (attr, result)

parseRules "gdl_token_array_2_nested" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "String") >>~ pushContext "gdl_token_array_2_nested")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_token_array_3" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "String") >>~ pushContext "gdl_token_array_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_token_array_3_nested" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "String") >>~ pushContext "gdl_token_array_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_token_array_4" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '>' >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "String") >>~ pushContext "gdl_token_array_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_token_array_4_nested" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "String") >>~ pushContext "gdl_token_array_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_token_array_5" = 
  do (attr, result) <- (((parseRules "token_array_rules"))
                        <|>
                        ((pRegExprDynamic "\\\\%1" >>= withAttribute "String"))
                        <|>
                        ((pRegExprDynamic "\\s*%1" >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "token_array_rules" = 
  do (attr, result) <- ((pString False "\\\\" >>= withAttribute "String"))
     return (attr, result)

parseRules "gdl_apostrophed_1" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Raw String"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_apostrophed_1_nested" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Raw String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_apostrophed_2" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '}' >>= withAttribute "Raw String"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_2_nested"))
     return (attr, result)

parseRules "gdl_apostrophed_2_nested" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_2_nested")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Raw String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_apostrophed_3" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Raw String"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_apostrophed_3_nested" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Raw String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_apostrophed_4" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '>' >>= withAttribute "Raw String"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_apostrophed_4_nested" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Raw String") >>~ pushContext "gdl_apostrophed_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Raw String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_apostrophed_5" = 
  do (attr, result) <- (((parseRules "apostrophed_rules"))
                        <|>
                        ((pRegExprDynamic "\\\\%1" >>= withAttribute "Raw String"))
                        <|>
                        ((pRegExprDynamic "\\s*%1" >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "apostrophed_rules" = 
  do (attr, result) <- ((pDetect2Chars False '\\' '\\' >>= withAttribute "Raw String"))
     return (attr, result)

parseRules "gdl_shell_command_1" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Command"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_shell_command_1_nested" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Command") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_shell_command_2" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '}' >>= withAttribute "Command"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_2_nested"))
     return (attr, result)

parseRules "gdl_shell_command_2_nested" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_2_nested")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Command") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_shell_command_3" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Command"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_shell_command_3_nested" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Command") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_shell_command_4" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '>' >>= withAttribute "Command"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_shell_command_4_nested" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Command") >>~ pushContext "gdl_shell_command_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Command") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_shell_command_5" = 
  do (attr, result) <- (((parseRules "shell_command_rules"))
                        <|>
                        ((pRegExprDynamic "\\\\%1" >>= withAttribute "Command"))
                        <|>
                        ((pRegExprDynamic "\\s*%1" >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "shell_command_rules" = 
  do (attr, result) <- (((pDetect2Chars False '\\' '\\' >>= withAttribute "Command"))
                        <|>
                        ((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution") >>~ pushContext "Short Subst")
                        <|>
                        ((pDetect2Chars False '#' '{' >>= withAttribute "Substitution") >>~ pushContext "Subst"))
     return (attr, result)

parseRules "gdl_regexpr_1" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ')' >>= withAttribute "Regular Expression"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_1_nested")
                        <|>
                        ((pRegExpr (compileRegex "\\)[uiomxn]*") >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_regexpr_1_nested" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetectChar False '(' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_1_nested")
                        <|>
                        ((pDetectChar False ')' >>= withAttribute "Regular Expression") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_regexpr_2" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '}' >>= withAttribute "Regular Expression"))
                        <|>
                        ((pRegExpr (compileRegex "\\}[uiomxn]*") >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_2_nested"))
     return (attr, result)

parseRules "gdl_regexpr_2_nested" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_2_nested")
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Regular Expression") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_regexpr_3" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetect2Chars False '\\' ']' >>= withAttribute "Regular Expression"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_3_nested")
                        <|>
                        ((pRegExpr (compileRegex "\\][uiomxn]*") >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_regexpr_3_nested" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetectChar False '[' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_3_nested")
                        <|>
                        ((pDetectChar False ']' >>= withAttribute "Regular Expression") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_regexpr_4" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetect2Chars False '\\' '>' >>= withAttribute "Regular Expression"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_4_nested")
                        <|>
                        ((pRegExpr (compileRegex ">[uiomxn]*") >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "gdl_regexpr_4_nested" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pDetectChar False '<' >>= withAttribute "Regular Expression") >>~ pushContext "gdl_regexpr_4_nested")
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "Regular Expression") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "gdl_regexpr_5" = 
  do (attr, result) <- (((parseRules "regexpr_rules"))
                        <|>
                        ((pRegExprDynamic "\\\\%1" >>= withAttribute "Regular Expression"))
                        <|>
                        ((pRegExprDynamic "\\s*%1[uiomxn]*" >>= withAttribute "GDL input") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "regexpr_rules" = 
  do (attr, result) <- (((pDetect2Chars False '\\' '\\' >>= withAttribute "Regular Expression"))
                        <|>
                        ((pRegExpr (compileRegex "#@{1,2}") >>= withAttribute "Substitution") >>~ pushContext "Short Subst")
                        <|>
                        ((pDetect2Chars False '#' '{' >>= withAttribute "Substitution") >>~ pushContext "Subst"))
     return (attr, result)

parseRules "DATA" = 
  pzero

parseRules x = fail $ "Unknown context" ++ x