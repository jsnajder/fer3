module CSV
  ( Field
  , ParseError
  , readCSV
  , showCSV
  , csvToForest ) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe
import Data.Tree
import Text.Parsec
import Text.Parsec.String

type Field = String

------------------------------------------------------------------------------
-- Reading a CSV

readCSV :: String -> Either ParseError [[Field]]
readCSV s = map pruneRow <$> parse parseCSV "" s

parseCSV :: Parser [[Field]]
parseCSV = parseRow `sepEndBy` eol

pruneRow :: [Field] -> [Field]
pruneRow = reverse . dropWhile null . reverse . map pruneCell

pruneCell :: Field -> Field
pruneCell s | all isSpace s = ""
            | otherwise     = s

parseRow :: Parser [Field]
parseRow = parseField `sepEndBy` char ','

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

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "eol"

------------------------------------------------------------------------------
-- Showing a CSV

showCSV :: [[Field]] -> String
showCSV = unlines . map (intercalate "," . map csvQuote)

csvQuote :: String -> String
csvQuote s | needsQuotes = "\"" ++ escape s ++ "\""
           | otherwise   = s
  where needsQuotes = any (==',') s
        escape []       = []
        escape ('"':xs) = "\"\"" ++ escape xs
        escape (x:xs)   = x : escape xs

------------------------------------------------------------------------------
-- Reading into a CSV

xs = [(0,"A"),(1,"B"),(1,"C"),(0,"E"),(1,"F"),(0,"G"),(1,"H")] :: [(Int,String)]

toForest :: [(Int,a)] -> Forest a
toForest []          = []
toForest ((lx,x):xs) = let (ys,zs) = break ((==lx) . fst) xs
                     in Node x (toForest ys) : toForest zs

csvToForest :: [[Field]] -> Forest [Field]
csvToForest = toForest . map (\r -> (indentLevel r,r)) 

indentLevel :: [Field] -> Int
indentLevel = length . takeWhile null

