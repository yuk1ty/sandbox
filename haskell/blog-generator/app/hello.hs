import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
        (h1_ "Heading")
        ( append_
            (p_ "Paragraph #1")
            (code_ "<html></html>")
        )
    )

