{- This module was generated from data in the Kate syntax highlighting file doxygen.xml, version 1.29,
   by  Dominik Haumann (dhdev@gmx.de) -}

module Text.Highlighting.Kate.Syntax.Doxygen ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import qualified Text.Highlighting.Kate.Syntax.Alert
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Doxygen"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.dox;*.doxygen"

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
  setState $ st { synStLanguage = "Doxygen" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ zipWith (\num line -> SourceLine num (normalizeHighlighting line)) [1..] result

startingState = SyntaxState {synStContexts = fromList [("Doxygen",["Normal"])], synStLanguage = "Doxygen", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = False, synStKeywordDelims = " \n\t.():!+,-<=>%&*/;?[]^{|}", synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "LineComment" -> (popContext >> return ())
    "BlockComment" -> return ()
    "ML_TagWord" -> (popContext >> return ())
    "ML_TagParam" -> (popContext >> return ())
    "ML_TagWordWord" -> (popContext >> return ())
    "ML_Tag2ndWord" -> (popContext >> popContext >> return ())
    "ML_TagString" -> (popContext >> return ())
    "ML_TagWordString" -> (popContext >> return ())
    "ML_htmltag" -> return ()
    "ML_htmlcomment" -> return ()
    "ML_identifiers" -> return ()
    "ML_types1" -> return ()
    "ML_types2" -> return ()
    "SL_TagWord" -> (popContext >> return ())
    "SL_TagParam" -> (popContext >> return ())
    "SL_TagWordWord" -> (popContext >> return ())
    "SL_Tag2ndWord" -> (popContext >> popContext >> return ())
    "SL_TagString" -> (popContext >> return ())
    "SL_TagWordString" -> (popContext >> return ())
    "SL_htmltag" -> (popContext >> return ())
    "SL_htmlcomment" -> (popContext >> return ())
    "SL_identifiers" -> (popContext >> return ())
    "SL_types1" -> (popContext >> return ())
    "SL_types2" -> (popContext >> return ())
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

styles = [("Normal Text","Normal"),("Tags","Keyword"),("Word","Keyword"),("HTML Tag","Keyword"),("Description","String"),("Comment","Comment"),("Region","RegionMarker"),("Identifier","Others"),("HTML Comment","Comment"),("Types","DataType")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("LineComment","Comment"),("BlockComment","Comment"),("ML_TagWord","Comment"),("ML_TagParam","Comment"),("ML_TagWordWord","Comment"),("ML_Tag2ndWord","Comment"),("ML_TagString","Comment"),("ML_TagWordString","Comment"),("ML_htmltag","Identifier"),("ML_htmlcomment","HTML Comment"),("ML_identifiers","Identifier"),("ML_types1","Types"),("ML_types2","Types"),("SL_TagWord","Comment"),("SL_TagParam","Comment"),("SL_TagWordWord","Comment"),("SL_Tag2ndWord","Comment"),("SL_TagString","Comment"),("SL_TagWordString","Comment"),("SL_htmltag","Identifier"),("SL_htmlcomment","HTML Comment"),("SL_identifiers","Identifier"),("SL_types1","Types"),("SL_types2","Types")]

parseRules "Normal" = 
  do (attr, result) <- (((pRegExpr (compileRegex "//(!|(/(?=[^/]|$)))<?") >>= withAttribute "Comment") >>~ pushContext "LineComment")
                        <|>
                        ((pRegExpr (compileRegex "/\\*(\\*[^*/]|!|[*!]<|\\*$)") >>= withAttribute "Comment") >>~ pushContext "BlockComment")
                        <|>
                        ((pRegExpr (compileRegex "//\\s*@\\{\\s*$") >>= withAttribute "Region"))
                        <|>
                        ((pRegExpr (compileRegex "//\\s*@\\}\\s*$") >>= withAttribute "Region"))
                        <|>
                        ((pRegExpr (compileRegex "/\\*\\s*@\\{\\s*\\*/") >>= withAttribute "Region"))
                        <|>
                        ((pRegExpr (compileRegex "/\\*\\s*@\\}\\s*\\*/") >>= withAttribute "Region")))
     return (attr, result)

parseRules "LineComment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pKeyword ["\\arg","\\attention","\\author","\\callgraph","\\code","\\dot","\\else","\\endcode","\\endcond","\\enddot","\\endhtmlonly","\\endif","\\endlatexonly","\\endlink","\\endmanonly","\\endverbatim","\\endxmlonly","\\f[","\\f]","\\f$","\\hideinitializer","\\htmlonly","\\interface","\\internal","\\invariant","\\~","\\@","\\$","\\\\","\\#","\\latexonly","\\li","\\manonly","\\n","\\nosubgrouping","\\note","\\only","\\post","\\pre","\\remarks","\\return","\\returns","\\sa","\\see","\\showinitializer","\\since","\\test","\\todo","\\verbatim","\\warning","\\xmlonly","@arg","@attention","@author","@callgraph","@code","@dot","@else","@endcode","@endcond","@enddot","@endhtmlonly","@endif","@endlatexonly","@endlink","@endmanonly","@endverbatim","@endxmlonly","@f[","@f]","@f$","@hideinitializer","@htmlonly","@interface","@internal","@invariant","@~","@@","@$","@\\","@#","@latexonly","@li","@manonly","@n","@nosubgrouping","@note","@only","@post","@pre","@remarks","@return","@returns","@sa","@see","@showinitializer","@since","@test","@todo","@verbatim","@warning","@xmlonly"] >>= withAttribute "Tags"))
                        <|>
                        ((pKeyword ["\\addtogroup","\\a","\\anchor","\\b","\\c","\\class","\\cond","\\copydoc","\\def","\\dontinclude","\\dotfile","\\e","\\elseif","\\em","\\enum","\\example","\\exception","\\exceptions","\\file","\\htmlinclude","\\if","\\ifnot","\\include","\\link","\\namespace","\\p","\\package","\\ref","\\relatesalso","\\relates","\\retval","\\throw","\\throws","\\verbinclude","\\version","\\xrefitem","@addtogroup","@a","@anchor","@b","@c","@class","@cond","@copydoc","@def","@dontinclude","@dotfile","@e","@elseif","@em","@enum","@example","@exception","@exceptions","@file","@htmlinclude","@if","@ifnot","@include","@link","@namespace","@p","@package","@ref","@relatesalso","@relates","@retval","@throw","@throws","@verbinclude","@version","@xrefitem"] >>= withAttribute "Tags") >>~ pushContext "SL_TagWord")
                        <|>
                        ((pKeyword ["\\param","@param"] >>= withAttribute "Tags") >>~ pushContext "SL_TagParam")
                        <|>
                        ((pKeyword ["\\image","@image"] >>= withAttribute "Tags") >>~ pushContext "SL_TagWordWord")
                        <|>
                        ((pKeyword ["\\addindex","\\brief","\\bug","\\date","\\deprecated","\\fn","\\ingroup","\\line","\\mainpage","\\name","\\overload","\\par","\\short","\\skip","\\skipline","\\typedef","\\until","\\var","@addindex","@brief","@bug","@date","@deprecated","@fn","@ingroup","@line","@mainpage","@name","@overload","@par","@short","@skip","@skipline","@typedef","@until","@var"] >>= withAttribute "Tags") >>~ pushContext "SL_TagString")
                        <|>
                        ((pKeyword ["\\defgroup","\\page","\\paragraph","\\section","\\struct","\\subsection","\\subsubsection","\\union","\\weakgroup","@defgroup","@page","@paragraph","@section","@struct","@subsection","@subsubsection","@union","@weakgroup"] >>= withAttribute "Tags") >>~ pushContext "SL_TagWordString")
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment"))
                        <|>
                        ((pString False "<!--" >>= withAttribute "HTML Comment") >>~ pushContext "SL_htmlcomment")
                        <|>
                        ((pDetect2Chars False '<' '<' >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[a-zA-Z_:][a-zA-Z0-9._:-]*") >>= withAttribute "HTML Tag") >>~ pushContext "SL_htmltag"))
     return (attr, result)

parseRules "BlockComment" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pDetect2Chars False '@' '{' >>= withAttribute "Region"))
                        <|>
                        ((pDetect2Chars False '@' '}' >>= withAttribute "Region"))
                        <|>
                        ((pKeyword ["\\arg","\\attention","\\author","\\callgraph","\\code","\\dot","\\else","\\endcode","\\endcond","\\enddot","\\endhtmlonly","\\endif","\\endlatexonly","\\endlink","\\endmanonly","\\endverbatim","\\endxmlonly","\\f[","\\f]","\\f$","\\hideinitializer","\\htmlonly","\\interface","\\internal","\\invariant","\\~","\\@","\\$","\\\\","\\#","\\latexonly","\\li","\\manonly","\\n","\\nosubgrouping","\\note","\\only","\\post","\\pre","\\remarks","\\return","\\returns","\\sa","\\see","\\showinitializer","\\since","\\test","\\todo","\\verbatim","\\warning","\\xmlonly","@arg","@attention","@author","@callgraph","@code","@dot","@else","@endcode","@endcond","@enddot","@endhtmlonly","@endif","@endlatexonly","@endlink","@endmanonly","@endverbatim","@endxmlonly","@f[","@f]","@f$","@hideinitializer","@htmlonly","@interface","@internal","@invariant","@~","@@","@$","@\\","@#","@latexonly","@li","@manonly","@n","@nosubgrouping","@note","@only","@post","@pre","@remarks","@return","@returns","@sa","@see","@showinitializer","@since","@test","@todo","@verbatim","@warning","@xmlonly"] >>= withAttribute "Tags"))
                        <|>
                        ((pKeyword ["\\addtogroup","\\a","\\anchor","\\b","\\c","\\class","\\cond","\\copydoc","\\def","\\dontinclude","\\dotfile","\\e","\\elseif","\\em","\\enum","\\example","\\exception","\\exceptions","\\file","\\htmlinclude","\\if","\\ifnot","\\include","\\link","\\namespace","\\p","\\package","\\ref","\\relatesalso","\\relates","\\retval","\\throw","\\throws","\\verbinclude","\\version","\\xrefitem","@addtogroup","@a","@anchor","@b","@c","@class","@cond","@copydoc","@def","@dontinclude","@dotfile","@e","@elseif","@em","@enum","@example","@exception","@exceptions","@file","@htmlinclude","@if","@ifnot","@include","@link","@namespace","@p","@package","@ref","@relatesalso","@relates","@retval","@throw","@throws","@verbinclude","@version","@xrefitem"] >>= withAttribute "Tags") >>~ pushContext "ML_TagWord")
                        <|>
                        ((pKeyword ["\\param","@param"] >>= withAttribute "Tags") >>~ pushContext "ML_TagParam")
                        <|>
                        ((pKeyword ["\\image","@image"] >>= withAttribute "Tags") >>~ pushContext "ML_TagWordWord")
                        <|>
                        ((pKeyword ["\\addindex","\\brief","\\bug","\\date","\\deprecated","\\fn","\\ingroup","\\line","\\mainpage","\\name","\\overload","\\par","\\short","\\skip","\\skipline","\\typedef","\\until","\\var","@addindex","@brief","@bug","@date","@deprecated","@fn","@ingroup","@line","@mainpage","@name","@overload","@par","@short","@skip","@skipline","@typedef","@until","@var"] >>= withAttribute "Tags") >>~ pushContext "ML_TagString")
                        <|>
                        ((pKeyword ["\\defgroup","\\page","\\paragraph","\\section","\\struct","\\subsection","\\subsubsection","\\union","\\weakgroup","@defgroup","@page","@paragraph","@section","@struct","@subsection","@subsubsection","@union","@weakgroup"] >>= withAttribute "Tags") >>~ pushContext "ML_TagWordString")
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\\\(<|>)") >>= withAttribute "Tags"))
                        <|>
                        ((pDetect2Chars False '<' '<' >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[a-zA-Z_:][a-zA-Z0-9._:-]*") >>= withAttribute "HTML Tag") >>~ pushContext "ML_htmltag")
                        <|>
                        ((pString False "<!--" >>= withAttribute "HTML Comment") >>~ pushContext "ML_htmlcomment"))
     return (attr, result)

parseRules "ML_TagWord" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "SL_TagWord")))
     return (attr, result)

parseRules "ML_TagParam" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pString False "[in]" >>= withAttribute "Tags") >>~ pushContext "ML_Tag2ndWord")
                        <|>
                        ((pString False "[out]" >>= withAttribute "Tags") >>~ pushContext "ML_Tag2ndWord")
                        <|>
                        ((pString False "[in,out]" >>= withAttribute "Tags") >>~ pushContext "ML_Tag2ndWord")
                        <|>
                        ((pRegExpr (compileRegex "\\S(?=([][,?;()]|\\.$|\\.?\\s))") >>= withAttribute "Word") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Word")))
     return (attr, result)

parseRules "ML_TagWordWord" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\S(?=([][,?;()]|\\.$|\\.?\\s))") >>= withAttribute "Word") >>~ pushContext "ML_Tag2ndWord")
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Word")))
     return (attr, result)

parseRules "ML_Tag2ndWord" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((parseRules "SL_Tag2ndWord")))
     return (attr, result)

parseRules "ML_TagString" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pString False "<!--" >>= withAttribute "HTML Comment") >>~ pushContext "ML_htmlcomment")
                        <|>
                        ((pDetect2Chars False '<' '<' >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[a-zA-Z_:][a-zA-Z0-9._:-]*") >>= withAttribute "HTML Tag") >>~ pushContext "ML_htmltag")
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "Description")))
     return (attr, result)

parseRules "ML_TagWordString" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((parseRules "SL_TagWordString")))
     return (attr, result)

parseRules "ML_htmltag" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetect2Chars False '/' '>' >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*=\\s*") >>= withAttribute "Identifier") >>~ pushContext "ML_identifiers"))
     return (attr, result)

parseRules "ML_htmlcomment" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pString False "-->" >>= withAttribute "HTML Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "ML_identifiers" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*#?[a-zA-Z0-9]*") >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Types") >>~ pushContext "ML_types1")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Types") >>~ pushContext "ML_types2"))
     return (attr, result)

parseRules "ML_types1" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Types") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "ML_types2" = 
  do (attr, result) <- (((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Types") >>~ (popContext >> popContext >> return ())))
     return (attr, result)

parseRules "SL_TagWord" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pKeyword ["\\addtogroup","\\a","\\anchor","\\b","\\c","\\class","\\cond","\\copydoc","\\def","\\dontinclude","\\dotfile","\\e","\\elseif","\\em","\\enum","\\example","\\exception","\\exceptions","\\file","\\htmlinclude","\\if","\\ifnot","\\include","\\link","\\namespace","\\p","\\package","\\ref","\\relatesalso","\\relates","\\retval","\\throw","\\throws","\\verbinclude","\\version","\\xrefitem","@addtogroup","@a","@anchor","@b","@c","@class","@cond","@copydoc","@def","@dontinclude","@dotfile","@e","@elseif","@em","@enum","@example","@exception","@exceptions","@file","@htmlinclude","@if","@ifnot","@include","@link","@namespace","@p","@package","@ref","@relatesalso","@relates","@retval","@throw","@throws","@verbinclude","@version","@xrefitem"] >>= withAttribute "Tags") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S(?=([][,?;()]|\\.$|\\.?\\s))") >>= withAttribute "Word") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Word")))
     return (attr, result)

parseRules "SL_TagParam" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pString False "[in]" >>= withAttribute "Tags") >>~ pushContext "SL_Tag2ndWord")
                        <|>
                        ((pString False "[out]" >>= withAttribute "Tags") >>~ pushContext "SL_Tag2ndWord")
                        <|>
                        ((pString False "[in,out]" >>= withAttribute "Tags") >>~ pushContext "SL_Tag2ndWord")
                        <|>
                        ((pRegExpr (compileRegex "\\S(?=([][,?;()]|\\.$|\\.?\\s))") >>= withAttribute "Word") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Word")))
     return (attr, result)

parseRules "SL_TagWordWord" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\S(?=([][,?;()]|\\.$|\\.?\\s))") >>= withAttribute "Word") >>~ pushContext "SL_Tag2ndWord")
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Word")))
     return (attr, result)

parseRules "SL_Tag2ndWord" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\S(?=([][,?;()]|\\.$|\\.?\\s))") >>= withAttribute "Word") >>~ (popContext >> popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Word")))
     return (attr, result)

parseRules "SL_TagString" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pString False "<!--" >>= withAttribute "HTML Comment") >>~ pushContext "SL_htmlcomment")
                        <|>
                        ((pDetect2Chars False '<' '<' >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "<\\/?[a-zA-Z_:][a-zA-Z0-9._:-]*") >>= withAttribute "HTML Tag") >>~ pushContext "SL_htmltag")
                        <|>
                        ((pRegExpr (compileRegex ".") >>= withAttribute "Description")))
     return (attr, result)

parseRules "SL_TagWordString" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "\\S(?=([][,?;()]|\\.$|\\.?\\s))") >>= withAttribute "Word") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\S") >>= withAttribute "Word")))
     return (attr, result)

parseRules "SL_htmltag" = 
  do (attr, result) <- (((pDetect2Chars False '/' '>' >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '>' >>= withAttribute "HTML Tag") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "\\s*=\\s*") >>= withAttribute "Identifier") >>~ pushContext "SL_identifiers"))
     return (attr, result)

parseRules "SL_htmlcomment" = 
  do (attr, result) <- (((Text.Highlighting.Kate.Syntax.Alert.parseExpression >>= ((withAttribute "") . snd)))
                        <|>
                        ((pString False "-->" >>= withAttribute "HTML Comment") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "SL_identifiers" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\s*#?[a-zA-Z0-9]*") >>= withAttribute "String") >>~ (popContext >> return ()))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "Types") >>~ pushContext "SL_types1")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "Types") >>~ pushContext "SL_types2"))
     return (attr, result)

parseRules "SL_types1" = 
  do (attr, result) <- ((pDetectChar False '\'' >>= withAttribute "Types") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules "SL_types2" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "Types") >>~ (popContext >> popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x