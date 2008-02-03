module Main where
import Text.Highlighting.Kate
import Text.Highlighting.Kate.Syntax (languages)
import System.IO
import System.Environment
import Text.XHtml.Transitional
import System.Console.GetOpt
import System.Exit
import Data.Maybe
import Data.Char (toLower)

data Flag = CssPath String
          | Help
          | List
          | NumberLines
          | TitleAttributes
          deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['c'] ["css"] (ReqArg CssPath "PATH") "link CSS file"
  , Option ['h'] ["help"] (NoArg Help)   "show usage message"
  , Option ['l'] ["list"] (NoArg List)   "list available languages"
  , Option ['n'] ["number-lines"] (NoArg NumberLines)  "number lines"
  , Option ['t'] ["title-attributes"] (NoArg TitleAttributes)  "include structure in title attributes"
  ]

cssPathOf :: [Flag] -> Maybe String
cssPathOf [] = Nothing
cssPathOf (CssPath s : _) = Just s
cssPathOf (_:xs) = cssPathOf xs

main = do
  (opts, args, errs) <- getArgs >>= return . getOpt Permute options 
  let head = "Highlight [options] language [files...]"
  if not (null errs)
     then ioError (userError $ concat errs ++ usageInfo head options)
     else return ()
  if List `elem` opts
     then hPutStrLn stderr (unwords languages) >> exitWith (ExitFailure 2)
     else return ()
  (lang:fnames) <- if null args || Help `elem` opts
                      then hPutStrLn stderr (usageInfo head options) >> 
                           exitWith (ExitFailure 1)
                      else return args
  if not ((map toLower lang) `elem` (map (map toLower) languages))
     then hPutStrLn stderr ("Unknown language " ++ lang) >> exitWith (ExitFailure 4)
     else return ()
  code <- if null fnames
             then getContents
             else mapM readFile fnames >>= return . concat
  let highlightOpts = (if TitleAttributes `elem` opts then [OptTitleAttributes] else []) ++
                      (if NumberLines `elem` opts then [OptNumberLines] else [])
  let css = fromMaybe "css/highlighting-kate.css" $ cssPathOf opts
  let hcode = xhtmlHighlight highlightOpts lang code
  let renderedHtml = renderHtml $ header << [thelink ! [thetype "text/css", href css, rel "stylesheet"] << noHtml] +++
                                  body << hcode
  putStrLn renderedHtml
