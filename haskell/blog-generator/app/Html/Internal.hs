module Html.Internal where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

type Title = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ =
  -- equals to `Structure(el("ul", concatMap(el("li", getStructureString(arguments))))`?
  Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString struct =
  case struct of
    Structure str -> str

instance Semigroup Structure where
  (<>) a b = Structure (getStructureString a <> getStructureString b)

empty_ :: Structure
empty_ = Structure ""

instance Monoid Structure where
  mempty = empty_
  mconcat list =
    case list of
      [] -> mempty
      x : xs -> x <> mconcat xs

render :: Html -> String
render html =
  case html of
    Html str -> str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          _ -> [c]
   in concatMap escapeChar
