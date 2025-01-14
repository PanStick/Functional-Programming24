isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouyAEIOUY"

countVowels :: String -> Int
countVowels = length . filter isVowel

answerQuestion :: String -> String
answerQuestion question
  | even (countVowels question) = "tak"
  | otherwise = "nie"

main :: IO ()
main = do
  putStrLn "Zadaj pytanie:"
  question <- getLine
  putStrLn $ answerQuestion question
  main
