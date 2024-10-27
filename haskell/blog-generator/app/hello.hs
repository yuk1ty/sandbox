import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( h1_ "heading"
        <> p_ "Paragraph #1"
        <> code_ "<html></html>"
    )
