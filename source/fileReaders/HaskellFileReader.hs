module HaskellFileReader where
  readInputOfDay :: Int -> IO String
  readInputOfDay day = do
    let dayInputPath = "source\\day" ++ show day ++ "\\input.txt" -- assumes it's ran from root
    readFile dayInputPath
