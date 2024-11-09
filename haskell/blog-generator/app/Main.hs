import Convert (convert, convertDirectory, convertSingle)
import qualified Html
import qualified Markup
import OptParse
import System.Directory (doesFileExist)
import System.Exit
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      convertDirectory input output
    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode
      outputHandle <-
        case output of
          StdOut -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure
      convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
