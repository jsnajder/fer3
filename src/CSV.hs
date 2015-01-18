module CSV
  ( Field
  , Row
  , CSV -- todo: rename to "table"
  , (!!!)
  , getField
  , getFieldMaybe
  , ParseError
  , readCSV
  , showCSV
  , printCSV
  , csvToForest ) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe
import Data.Tree
import Text.Parsec
import Text.Parsec.String

type Field = String
type Row   = [Field]
type CSV   = [Row]

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

printCSV :: CSV -> IO ()
printCSV = putStr . showCSV

csvQuote :: String -> String
csvQuote s | needsQuotes = "\"" ++ escape s ++ "\""
           | otherwise   = s
  where needsQuotes = any (==',') s
        escape []       = []
        escape ('"':xs) = "\"\"" ++ escape xs
        escape (x:xs)   = x : escape xs

------------------------------------------------------------------------------
-- Reading into a CSV

toForest :: [(Int,a)] -> Forest a
toForest []          = []
toForest ((lx,x):xs) = let (ys,zs) = break ((==lx) . fst) xs
                     in Node x (toForest ys) : toForest zs

csvToForest :: [[Field]] -> Forest [Field]
csvToForest = toForest . map (\r -> (indentLevel r,r)) 

indentLevel :: [Field] -> Int
indentLevel = length . takeWhile null

------------------------------------------------------------------------------
-- Accessing CSV fields

(!!!) :: [[a]] -> Int -> Maybe [a]
[]     !!! _ = Nothing
([]:_) !!! 0 = Nothing
(x:_)  !!! 0 = Just x
(_:xs) !!! n = xs !!! (n - 1)

getField :: CSV -> Int -> Int -> Maybe Field
getField ys y x =  (ys !!! y) >>= (!!! x)

getFieldMaybe :: CSV -> Int -> Int -> Field -> Field
getFieldMaybe ys y x f = fromMaybe f $ getField ys y x

