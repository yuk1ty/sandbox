type Item = String

type Items = [String]

-- Returns a new list of Items with the new item in it
addItem :: Item -> Items -> Items
addItem item items = item : items

-- Returns a string representation of items
displayItems :: Items -> String
displayItems items =
  let displayItem index item = show index ++ " - " ++ item
      reversedList = reverse items
      displayedItemsList = zipWith displayItem [1 ..] reversedList
   in unlines displayedItemsList

-- Returns a new list of items or an error message if the index is out of bounds
removeItem :: Int -> Items -> Either String Items
removeItem reverseIndex allItems =
  impl (length allItems - reverseIndex) allItems
 where
  impl index items =
    case (index, items) of
      (0, _ : rest) -> Right rest
      (_, []) -> Left "Index out of bounds"
      (n, item : rest) ->
        case impl (n - 1) rest of
          Right newItems ->
            Right (item : newItems)
          Left errMsg ->
            Left errMsg

-- commands
interactWithUser :: Items -> IO ()
interactWithUser items = do
  putStrLn "Commands: quit, items, add - <item to add>, done <index>"
  line <- getLine
  case parseCommand line of
    Right DisplayItems -> do
      putStrLn "The List of items is:"
      putStrLn (displayItems items)
      interactWithUser items
    Right (AddItem item) -> do
      let newItems = addItem item items
      putStrLn "Item added."
      interactWithUser newItems
    Right (Done index) -> do
      let result = removeItem index items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          interactWithUser items
        Right newItems -> do
          putStrLn "Item done."
          interactWithUser newItems
    Right Quit -> do
      putStrLn "Bye!"
      pure ()
    Left errMsg -> do
      putStrLn ("Error: " ++ errMsg)
      interactWithUser items

data Command
  = Quit
  | DisplayItems
  | AddItem String
  | Done Int

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["items"] -> Right DisplayItems
  "add" : "-" : item -> Right (AddItem (unwords item))
  ["done", idxStr] ->
    if all (`elem` "0123456789") idxStr
      then Right (Done (read idxStr))
      else Left "Invalid index"
  _ -> Left "Unknown command."

main :: IO ()
main = do
  putStrLn "TODO app"
  let initialList = []
  interactWithUser initialList
  putStrLn "Thnaks for using this app."
