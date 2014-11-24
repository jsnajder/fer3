{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Catalogue where

import Control.Applicative ((<$>))
import CSV
import qualified Data.EdgeLabeledGraph as G
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Tree
import Text.Parsec
import Text.Parsec.String
import Text.Printf

data ItemType = CAT | KA | KU | KT deriving (Eq,Show,Enum,Read,Ord)

type ItemEditor = String

data ItemId = ItemId 
  { catCode  :: String
  , areaCode :: String
  , unitId   :: Maybe Int
  , topicId  :: Maybe Int } deriving (Eq,Ord,Read,Show)

data Item = Item
  { itemId      :: ItemId
  , itemType    :: ItemType
  , itemLabel   :: String
  , itemVersion :: Maybe String
  , itemEditors :: [ItemEditor]
  , itemRemark  :: Maybe String } deriving (Eq,Show,Read,Ord)

data Link
  = SubItem 
  | Related
  | Prereq
  | Alias deriving (Eq,Ord,Show,Read)

type ItemIndex = T.Text

instance G.Vertex Item T.Text where
  index = T.pack . showItemId . itemId

type Catalogue = G.Graph ItemIndex Item Link

showItemId :: ItemId -> String
showItemId (ItemId c a (Just u) (Just t)) = printf "%s-%s%02d%03d" c a u t
showItemId (ItemId c a (Just u) Nothing) = printf "%s-%s%02d" c a u
showItemId (ItemId c a Nothing Nothing) = printf "%s-%s" c a

readItemId :: String -> Maybe ItemId
readItemId s = case parse parseItemId "" s of
  Right id -> Just id
  _        -> Nothing

parseItemId :: Parser ItemId
parseItemId = do
  c <- many letter
  char '-'
  a <- many letter
  u <- optionMaybe $ count 2 digit
  t <- optionMaybe $ count 2 digit
  return $ ItemId c a (read <$> u) (read <$> t)

readCatalogue :: String -> Either ParseError Catalogue
readCatalogue s = 
  G.unions . map (G.fromTree (const SubItem)) . 
  map (levelMap readItem) . csvToForest . filter (not . null) . drop 9 
  <$> readCSV s

readCatalogue2 :: String -> Either ParseError (Forest Item)
readCatalogue2 s = 
  map (levelMap readItem) . csvToForest . filter (not . null) . drop 9 
  <$> readCSV s

readItem :: Int -> [Field] -> Item
readItem 0 = readFields KA [0,1,6,7]
readItem 1 = readFields KU [2,3,6,7]
readItem 2 = readFields KT [4,5,6,7]

readFields t ix xs = Item
  { itemType    = t
  , itemId      = fromJust . readItemId $ xs !! (ix !! 0)
  , itemLabel   = xs !! (ix !! 1)
  , itemVersion = Nothing
  , itemRemark  = xs !!! (ix !! 2)
  , itemEditors = [xs !! (ix !! 3)] }

(!!!) :: [Field] -> Int -> Maybe Field
[]      !!! 0 = Nothing
([]:_ ) !!! 0 = Nothing
(x:_ )  !!! 0 = Just x
(_:xs)  !!! n = xs !!! (n - 1)

{- PUH:
(!!!) :: [a] -> Int -> Maybe a
[]     !!! 0 = Nothing
(x:_ ) !!! 0 = Just x
(_:xs) !!! n = xs !!! (n - 1)
-}


levelMap2 :: [a -> a] -> Tree a -> Tree a
levelMap2 [] n = n
levelMap2 (f:fs) (Node x ns) = Node (f x) (map (levelMap2 fs) ns)

levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f = lmap 0 
  where lmap l (Node x ns) = Node (f l x) (map (lmap $ l+1) ns)

