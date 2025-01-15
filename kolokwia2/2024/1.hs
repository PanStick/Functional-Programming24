
countVowels :: String -> Int
countVowels = length . filter (`elem` "aeiouyAEIOUY")

answerQuestion :: String -> String
answerQuestion question
  | even $ countVowels question = "tak"
  | otherwise = "nie"

main :: IO ()
main = do
  putStrLn "Zadaj pytanie:"
  question <- getLine
  putStrLn $ answerQuestion question
  main
