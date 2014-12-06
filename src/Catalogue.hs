{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Catalogue 
  ( Catalogue (..) -- <== TODO: don't expose the graph structure!!!
  , Item (..)
  , ItemId (..)
  , ItemEditor
  , loadCatalogue
  , readCatalogue
  , readItemId
  , readCatForest --tmp
  , showItemId
  , splitCatalogue
  , knowledgeItems
  , knowledgeAreas
  , getItem
  , getItemTree ) where

import Control.Applicative ((<$>))
import CSV
import qualified Data.EdgeLabeledGraph as G
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Text as T
import Data.List.Split (splitOneOf)
import Data.Tree
import Text.Parsec
import Text.Parsec.String
import Text.Printf

-- TODO: Abstract Catalogue interface (it shouldn't be exposed as a graph,
-- i.e., hide some constructors and provide operators to get items)

data ItemType = CAT | KA | KU | KT deriving (Eq,Show,Enum,Read,Ord)

type ItemEditor = String

data ItemId = ItemId 
  { catCode  :: String
  , areaCode :: String
  , unitId   :: Maybe Int
  , topicId  :: Maybe Int } deriving (Eq,Ord,Read)

instance Show ItemId where
  show = showItemId

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

data Catalogue = Cat 
  { catAreas   :: G.Graph ItemIndex Item Link
  , catId      :: String
  , catName    :: String
  , catVersion :: Maybe String
  , catDate    :: Maybe String
  , catEditors :: [String]
  , catRemarks :: Maybe String } deriving (Eq,Ord,Show,Read)

showItemId :: ItemId -> String
showItemId (ItemId c a (Just u) (Just t)) = printf "%s-%s%02d%02d" c a u t
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

loadCatalogue :: FilePath -> IO (Either ParseError Catalogue)
loadCatalogue f = readCatalogue <$> readFile f

readCatalogue :: String -> Either ParseError Catalogue
readCatalogue s = readCat <$> readCSV s

readCat :: CSV -> Catalogue
readCat xs = Cat
  { catAreas   = G.unions . map (G.fromTree (const SubItem)) . 
                 map (levelMap readItem) . csvToForest . 
                 filter (not . null) $ drop 9 xs
  , catId      = fromJust $ getField xs 1 2
  , catName    = fromMaybe "" $ getField xs 2 2
  , catVersion = getField xs 3 2
  , catDate    = getField xs 4 2
  , catEditors = concat . maybeToList $ ( readEditors <$> getField xs 5 2 )
  , catRemarks = getField xs 6 2 }

dummyCat :: Catalogue
dummyCat = Cat G.empty "" "" Nothing Nothing [] Nothing

-- tmp
readCatForest :: String -> Either ParseError (Forest Item)
readCatForest s = 
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
  , itemEditors = readEditors $ xs !! (ix !! 3) }

readEditors :: String -> [ItemEditor]
readEditors = map (unwords . words) . splitOneOf ",;"

levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f = lmap 0 
  where lmap l (Node x ns) = Node (f l x) (map (lmap $ l+1) ns)

-- splits catalogue according to areas
splitCatalogue :: Catalogue -> [Catalogue]
splitCatalogue =
  map (\es@(e:_) -> dummyCat { catAreas = G.fromEdgeList es, 
                               catId = outCatCode e }) .  
  groupBy ((==) `on` outCatCode) . 
  sortBy (compare `on` outCatCode) . 
  filter inCat .
  G.toEdgeList  . catAreas
  where outCatCode (v,_,_) = catCode $ itemId v
        inCat (v1,_,v2) = catCode (itemId v1) == catCode (itemId v2)

catCodes :: Catalogue -> [String]
catCodes = nub . sort . map (catCode . itemId) . G.vertices . catAreas

getItem :: Catalogue -> ItemId -> Maybe Item
getItem c x = G.vertex (T.pack $ showItemId x) (catAreas c)

getItemTree :: Catalogue -> ItemId -> Maybe (Tree Item)
getItemTree c x = (\x -> G.toTree x (==SubItem) (catAreas c)) <$> getItem c x

knowledgeItems :: Catalogue -> [Item]
knowledgeItems = G.vertices . catAreas

knowledgeAreas :: Catalogue -> [Item]
knowledgeAreas = filter ((==KA) . itemType) . knowledgeItems


