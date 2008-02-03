module Main where
import Text.Highlighting.Kate
import System.IO
import System.Environment
import Text.XHtml.Transitional

main = do
  argv <- getArgs
  if length argv < 2
     then error "Usage: two arguments, language name & file name"
     else return ()
  let lang = argv !! 0
  let fname = argv !! 1
  code <- readFile fname
  let hcode = xhtmlHighlight [OptNumberLines, OptTitleAttributes] lang code
  let renderedHtml = renderHtml $ header << [thelink ! [thetype "text/css", href "css/highlighting-kate.css", rel "stylesheet"] << noHtml] +++
                                  body << hcode
  putStrLn renderedHtml


