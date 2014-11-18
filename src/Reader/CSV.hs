module Reader.CSV where

import Data.List.Split (splitOn,splitOneOf)
import Text.Parsec hiding (label,labels)
import Text.Parsec.String

stringCell :: Parser String
stringCell = do
  s <- quotedCell <|> many1 (noneOf ",\n\r")
  char ',' <|> lookAhead newline
  return s

quotedCell :: Parser String
quotedCell = 
  do char '"'
     content <- many quotedChar
     char '"' <?> "quote at end of cell"
     return content

quotedChar :: Parser Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

nextLine :: Parser ()
nextLine = anyChar `manyTill` newline >> return () 

csvQuote :: String -> String
csvQuote s | needsQuotes = "\"" ++ escape s ++ "\""
           | otherwise = s
  where needsQuotes = any (==',') s
        escape [] = []
        escape ('"':xs) = "\"\"" ++ escape xs
        escape (x:xs)   = x : escape xs

stringSequence :: String -> [String]
stringSequence [] = []
stringSequence s  = map (unwords . words) $ splitOneOf ",;" s

emptyRow :: Parser ()
emptyRow = manyTill (char ',') newline >> return ()

