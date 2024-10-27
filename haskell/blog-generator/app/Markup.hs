module Markup (Document, Structure (..)) where

import Numeric.Natural

type Document =
  [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let paragraph = Paragraph (unlines (reverse currentParagraph))
   in case txts of
        [] -> [paragraph]
        currentLine : rest ->
          if trim currentLine == ""
            then
              paragraph : parseLines [] rest
            else
              parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words
