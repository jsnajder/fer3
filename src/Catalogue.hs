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
  , getItemTree
  , getItemArea 
  , csvKnowledgeUnit ) where

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

type CatalogueTree = Tree Item

getItemTree :: Catalogue -> ItemId -> Maybe CatalogueTree
getItemTree c x = (\x -> G.toTree x (==SubItem) (catAreas c)) <$> getItem c x

-- katastrofalan kod...
--getItemParent :: Catalogue -> ItemId -> Maybe Item
getItemParent c x =
  (listToMaybe . map (readItemId . T.unpack . snd) . filter ((==SubItem) . fst) . G.inEdges' (T.pack $ showItemId x) $ catAreas c) 
--  >>= getItem c . readItemId

-- with path from the root
getItemTree' :: Catalogue -> ItemId -> Maybe CatalogueTree
getItemTree' c x = undefined --getItemTree c x

knowledgeItems :: Catalogue -> [Item]
knowledgeItems = G.vertices . catAreas

knowledgeAreas :: Catalogue -> [Item]
knowledgeAreas = filter ((==KA) . itemType) . knowledgeItems

-- TODO: generalize so that it works with topics and areas...
addArea :: Catalogue -> CatalogueTree -> CatalogueTree
addArea c n@(Node x ns) = case getItemArea c (itemId x) of
  Nothing -> n
  Just x' -> Node x' [n]

getItemArea :: Catalogue -> ItemId -> Maybe Item
getItemArea c x = getItem c $ x { unitId = Nothing, topicId = Nothing }

csvKnowledgeUnit :: Catalogue -> ItemId -> Maybe CSV
csvKnowledgeUnit c x = csv . addArea c <$> getItemTree c x
  where csv (Node x ns) = csvItem x : concatMap csv ns
        
csvItem :: Item -> Row
csvItem x
  | itemType x == KA =
      [showItemId $ itemId x,itemLabel x,"","","","",
       fromMaybe "" $ itemRemark x,showItemEditors x]
  | itemType x == KU =
      ["","",showItemId $ itemId x,itemLabel x,"","",
       fromMaybe "" $ itemRemark x,showItemEditors x]
  | itemType x == KT =
      ["","","","",showItemId $ itemId x,itemLabel x,
       fromMaybe "" $ itemRemark x,showItemEditors x]
        
showItemEditors :: Item -> String
showItemEditors = intercalate ", " . itemEditors

-- todo: csvCatalogue

modifyItem :: Catalogue -> ItemId -> (Item -> Item) -> Catalogue
modifyItem c x f =
  c { catAreas = G.modifyVertex g $ catAreas c }
  where g y | itemId y == x = Just $ f y
            | otherwise     = Nothing

addItemRemark :: Catalogue -> ItemId -> String -> Catalogue
addItemRemark c x r = modifyItem c x addRemark
  where addRemark x = x  
          { itemRemark = case itemRemark x of
              Nothing -> Just r 
              Just r' -> Just $ r ++ "/n" ++ r' }

