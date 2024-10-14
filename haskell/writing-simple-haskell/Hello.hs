module Main where

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  let out = "Nice to meet you, " ++ name ++ "!"
  putStrLn out
