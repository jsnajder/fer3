module Reader.CSV where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Text.Parsec hiding (label,labels)
import Text.Parsec.String

type Field = String

readCSV :: String -> Either ParseError [[Field]]
readCSV s = map pruneRow <$> parse parseCSV "(unknown)" s

parseCSV :: Parser [[Field]]
parseCSV = parseRow `sepBy` newline

pruneRow :: [Field] -> [Field]
pruneRow s = case map pruneCell s of
  [[]] -> []
  xs   -> xs

pruneCell :: Field -> Field
pruneCell s | all isSpace s = ""
            | otherwise     = s

parseRow :: Parser [Field]
parseRow = parseField `sepBy` char ','

parseField :: Parser Field
parseField = quotedString <|> many (noneOf ",\n\r")

quotedString :: Parser String
quotedString = 
  do char '"'
     content <- many quotedChar
     char '"' <?> "quote at end of cell"
     return content

quotedChar :: Parser Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

------

csvQuote :: String -> String
csvQuote s | needsQuotes = "\"" ++ escape s ++ "\""
           | otherwise = s
  where needsQuotes = any (==',') s
        escape [] = []
        escape ('"':xs) = "\"\"" ++ escape xs
        escape (x:xs)   = x : escape xs

{-
stringSequence :: String -> [String]
stringSequence [] = []
stringSequence s  = map (unwords . words) $ splitOneOf ",;" s
-}

