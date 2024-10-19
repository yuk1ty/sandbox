main :: IO ()
main = putStrLn myhtml

myhtml :: String
myhtml = makeHtml "Hello title" (h1_ "Hello, title" <> p_ "Let's learn about Haskell")

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

newtype Html = Html String

newtype Structure = Structure String

html_ :: String -> String
html_ = el "html"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

body_ :: String -> String
body_ = el "body"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString struct =
  case struct of
    Structure str -> str
