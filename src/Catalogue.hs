{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Catalogue 
  ( Catalogue (..) -- <== TODO: don't expose the graph structure!!!
  , CatalogueComponent 
  , Item (..)
  , ItemType (..)
  , itemType'
  , ItemId (..)
  , Link (..) -- <== tmp
  , ItemEditor
  , loadCatalogue
  , readCatalogue
  , loadCatalogueComponent
  , readCatalogueComponent
  , readItemId
  , showItemId
  , splitCatalogue
  , knowledgeItems
  , knowledgeAreas
  , knowledgeUnits
  , getItem
  , getItemTree
  , getItemArea 
  , csvCatalogue
  , csvCatalogueSubset
  , saveCatalogue
  , fixItemIds
  , fixTreeItemIds
  , itemLinks
  , itemLinks'
  , addItemRemark
  , modifyItem
  , removeItem
  , removeItem'
  , removeItems
  , removeItems'
  , findItem
  , getSubitems'
  , getSubitems
  , getAllSubitems
  , getAllSubitems'
  , getParentItem
  , parentId
  , addItem
  , addItems
  , pruneItems
  , redirectLinks
  , redirectLinks'
  , replaceItem
  , replaceItem'
  , addItemTree
  , treesWithItems ) where

import Control.Applicative ((<$>),liftA2)
import Control.Monad
import CSV
import qualified Data.EdgeLabeledGraph as G  -- TMP!
import Data.Graph.Inductive
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Function
import qualified Data.Text as T
import Data.List.Split (splitOneOf)
import Data.Tree
import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import Text.Printf

-- TODO: Abstract Catalogue interface (it shouldn't be exposed as a graph,
-- i.e., hide some constructors and provide operators to get items)
-- TODO: better error handling when reading in the catalogue
-- TODO: switch foo' and foo semantics (primed versions take values, non-primed
--       versions take ids, which is the default)
-- TODO: Rename: CataologueTree -> ItemTree

------------------------------------------------------------------------------
-- Catalogue data structures
------------------------------------------------------------------------------

data ItemType = CAT | KA | KU | KT deriving (Eq,Show,Enum,Read,Ord)

type ItemEditor = String

data ItemId = ItemId 
  { catCode  :: String
  , areaCode :: String
  , unitId   :: Maybe Int
  , topicId  :: Maybe Int } deriving (Eq,Ord,Read,Show)

{-
instance Show ItemId where
  show = showItemId
-}

data Item = Item
  { itemId      :: ItemId
  , itemType    :: ItemType
  , itemLabel   :: String
  , itemVersion :: Maybe String
  , itemEditors :: [ItemEditor]
  , itemRemark  :: Maybe String } deriving (Eq,Show,Read,Ord)

itemType' :: ItemId -> ItemType
itemType' x = case (unitId x, topicId x) of
  (Just _, Just _ ) -> KT
  (Just _, Nothing) -> KU
  _                 -> KA

{-
rootNode :: Item
rootNode = Item
  { itemId = ItemId { catCode = "FER3"
                    , areaCode = "", unitId = Nothing, topicId = Nothing }
  , itemType    = CAT
  , itemLabel   = ""
  , itemVersion = Nothing
  , itemEditors = []
  , itemRemark  = Nothing }
-}

data Link
  = SubItem
  | Overlaps
  | Related
  | Prereq
  | ReplacedBy
  | Alias deriving (Eq,Ord,Show,Read)

type ItemIndex = T.Text

instance G.Vertex Item T.Text where
  index = T.pack . showItemId . itemId

type CatGraph = (NodeMap Item, Gr Item Link)

data Catalogue = Cat 
  { catAreas   :: G.Graph ItemIndex Item Link
  , catId      :: String
  , catName    :: String
  , catVersion :: Maybe String
  , catDate    :: Maybe String
  , catEditors :: [String]
  , catRemarks :: Maybe String } deriving (Eq,Ord,Show,Read)

emptyCat :: Catalogue
emptyCat = Cat G.empty "" "" Nothing Nothing [] Nothing

showItemId :: ItemId -> String
showItemId (ItemId c a (Just u) (Just t)) = printf "%s-%s%02d%02d" c a u t
showItemId (ItemId c a (Just u) Nothing) = printf "%s-%s%02d" c a u
showItemId (ItemId c a Nothing Nothing) = printf "%s-%s" c a

------------------------------------------------------------------------------
-- Catalogue reading from a CSV file
------------------------------------------------------------------------------

readItemId :: String -> Maybe ItemId
readItemId s = case parse parseItemId "" s of
  Right id -> Just id
  _        -> {-trace ("CANNOT READ: " ++ show s) $-} Nothing -- error $ "Cannot read ItemId " ++ show s

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
readCatalogue s = (\(c,ls) -> fromJust $ addLinks c ls) . readCat <$> readCSV s

-- "weak links" are yet non-instantiated links
type WeakLink = (ItemId,ItemId,Link)
type CatalogueComponent = (Catalogue, [WeakLink])

readCatalogueComponent :: String -> Either ParseError CatalogueComponent
readCatalogueComponent s = readCat <$> readCSV s

loadCatalogueComponent :: FilePath -> IO (Either ParseError CatalogueComponent)
loadCatalogueComponent f = readCatalogueComponent <$> readFile f

readCat :: CSV -> (Catalogue, [WeakLink])
readCat xs = (,links) $ Cat
  { catAreas    = G.unions $ map (G.fromTree $ const SubItem) forest'
  , catId       = getFieldMaybe xs 1 1 ""
  , catName     = getFieldMaybe xs 2 1 ""
  , catVersion  = getField xs 3 1
  , catDate     = getField xs 4 1
  , catEditors  = concat . maybeToList $ ( readEditors <$> getField xs 5 1 )
  , catRemarks  = getField xs 6 1 }
  where p []    = True
        p (x:_) = isNothing $ readItemId x
        forest  = map (levelMap readLinkedItem) . csvToForest . 
                       filter (not . null) $ dropWhile p xs
        forest' = map (fmap fst) forest
        areas   = map rootLabel forest'
        links   = concatMap (getLinks . flatten) forest

getLinks :: [LinkedItem] -> [(ItemId,ItemId,Link)]       
getLinks = concatMap (\(x1,ls) -> [(itemId x1,x2,l) | (l,x2) <- ls])
 
type LinkedItem = (Item,[(Link,ItemId)])

readLinkedItem :: Int -> [Field] -> LinkedItem
readLinkedItem level xs = (x, overlaps ++ replacedBy)
  where (x,zs)     = readItem level xs
        links l ix = map (l,) $ readItemIds ix zs
        overlaps   = links Overlaps 0   -- column I
        replacedBy = links ReplacedBy 1 -- column J
        
readItemIds :: Int -> [Field] -> [ItemId]
readItemIds ix xs = case xs !!! ix of
  Nothing -> []
  Just x  -> map (fromJust . readItemId . unwords . words) $ splitOneOf ",;" x

-- Reads item and the remaining columns
readItem :: Int -> [Field] -> (Item,[Field])
readItem level xs = case level of
  0 -> (readFields KA [0,1,6,7] xs, rest)
  1 -> (readFields KU [2,3,6,7] xs, rest)
  2 -> (readFields KT [4,5,6,7] xs, rest)
  where rest = drop 8 xs

readFields :: ItemType -> [Int] -> [Field] -> Item
readFields t ix xs = Item
  { itemType    = t
  , itemId      = fromJust . readItemId $ xs !! (ix !! 0)
  , itemLabel   = xs !! (ix !! 1)
  , itemVersion = Nothing
  , itemRemark  = xs !!! (ix !! 2)
  , itemEditors = readEditors . fromMaybe [] $ xs !!! (ix !! 3) }

readEditors :: String -> [ItemEditor]
readEditors = map (unwords . words) . splitOneOf ",;"

levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f = lmap 0 
  where lmap l (Node x ns) = Node (f l x) (map (lmap $ l+1) ns)

------------------------------------------------------------------------------
-- Catalogue access/modification
------------------------------------------------------------------------------

getItem :: Catalogue -> ItemId -> Maybe Item
getItem c x = G.vertex (T.pack $ showItemId x) (catAreas c)

addLink :: Catalogue -> (ItemId,ItemId,Link) -> Maybe Catalogue
addLink cat (x1,x2,l) = case (getItem cat x1, getItem cat x2) of
  (Just x1, Just x2) -> Just $ cat { catAreas = G.addEdge x1 x2 l (catAreas cat) }
  _                  -> error $ "Cannot add link " ++ show (x1,x2,l) --Nothing

addLinks :: Catalogue -> [(ItemId,ItemId,Link)] -> Maybe Catalogue
addLinks cat = foldM addLink cat

findItem :: Catalogue -> (Item -> Bool) -> Maybe Item
findItem cat p = find p $ knowledgeItems cat

getSubitems :: Catalogue -> Item -> [Item]
getSubitems cat x =
  mapMaybe (getItem cat . snd) . filter ((==SubItem) . fst) $ itemLinks cat x

getSubitems' :: Catalogue -> ItemId -> Maybe [Item]
getSubitems' cat x = getSubitems cat <$> getItem cat x

getAllSubitems :: Catalogue -> Item -> [Item]
getAllSubitems cat x | null xs   = xs
                     | otherwise = nub $ xs ++ concatMap (getSubitems cat) xs
  where xs = getSubitems cat x

getAllSubitems' :: Catalogue -> ItemId -> Maybe [Item]
getAllSubitems' cat x = getAllSubitems cat <$> getItem cat x

-- dual of: childId
parentId :: ItemId -> ItemId
parentId x = case (unitId x,topicId x) of
  (Just _, Just _ ) -> x { topicId = Nothing }
  _                 -> x { unitId = Nothing }

-- gets parent item by id
getParentItem' :: Catalogue -> ItemId -> Maybe Item
getParentItem' c = getItem c . parentId

-- gets parent item by id
getParentItem :: Catalogue -> Item -> Maybe Item
getParentItem c = getParentItem' c . itemId

modifyItem :: Catalogue -> ItemId -> (Item -> Item) -> Catalogue
modifyItem c x f = case getItem c x of
  Just x' -> c { catAreas = G.modifyVertex x' f $ catAreas c }
  Nothing -> c

-- removes an item and all its subitems as well all links pointing to 
-- it and the subitems
-- (removing dangling edges is taken care of by 'removeVertex')
removeItem :: Catalogue -> Item -> Catalogue
removeItem c x = c { catAreas = G.removeVertices (x:xs) (catAreas c) }
  where xs = getAllSubitems c x

removeItem' :: Catalogue -> ItemId -> Catalogue
removeItem' c x = fromMaybe c $ removeItem c <$> getItem c x

removeItems :: Catalogue -> [Item] -> Catalogue
removeItems c = foldl' removeItem c

removeItems' :: Catalogue -> [ItemId] -> Catalogue
removeItems' c = foldl' removeItem' c

-- adds an item and links it to its parent by ItemId
-- returns Nothing if the parent doesn't exist
-- NB: Knowlede areas have no parents and hence can always be added!
-- NB: Will replace existing items!
addItem :: Catalogue -> Item -> Maybe Catalogue
addItem c x 
  | itemType x == KA = Just $ c { catAreas = G.addVertex x (catAreas c) }
  | otherwise        =  
      case getParentItem c x of
      Just p  -> Just $ c { catAreas = G.addEdge p x SubItem (catAreas c) }
      Nothing -> Nothing

addItems :: Catalogue -> [Item] -> Maybe Catalogue
addItems c = foldM addItem c

-- remove unpopulated areas an units
pruneItems :: Catalogue -> Catalogue
pruneItems c = removeItems c2 ka
  where ku = filter (null . getSubitems c) $ knowledgeUnits c
        c2 = removeItems c ku
        ka = filter (null . getSubitems c) $ knowledgeAreas c

-- redirects incoming links to a new node
-- Subitem links are NOT redirected
redirectLinks :: Catalogue -> Item -> Item -> Catalogue
redirectLinks c x1 x2 = c { catAreas = G.modifyEdges f (catAreas c) }
  where f e@(_,SubItem,_)       = Just e
        f e@(x,l,y) | y==x1     = Just (x,l,x2)
                    | otherwise = Just e
 
redirectLinks' :: Catalogue -> ItemId -> ItemId -> Maybe Catalogue
redirectLinks' c x1 x2 = 
  liftA2 (redirectLinks c) (getItem c x1) (getItem c x2)

-- replaces one item by the other: item x1 is deleted and all links
-- (except SubItem links) that were pointing to x1 get redirected to x2
replaceItem :: Catalogue -> Item -> Item -> Catalogue
replaceItem c x1 x2 = removeItem (redirectLinks c x1 x2) x1 

replaceItem' :: Catalogue -> ItemId -> ItemId -> Maybe Catalogue
replaceItem' c x1 x2 = do
  c2 <- redirectLinks' c x1 x2
  return $ removeItem' c2 x1 

-- splits catalogue according to areas
splitCatalogue :: Catalogue -> [Catalogue]
splitCatalogue =
  map (\es@(e:_) -> emptyCat { catAreas = G.fromEdgeList es, 
                               catId = outCatCode e }) .  
  groupBy ((==) `on` outCatCode) . 
  sortBy (compare `on` outCatCode) . 
  filter inCat .
  G.toEdgeList  . catAreas
  where outCatCode (v,_,_) = catCode $ itemId v
        inCat (v1,_,v2) = catCode (itemId v1) == catCode (itemId v2)

catCodes :: Catalogue -> [String]
catCodes = nub . sort . map (catCode . itemId) . G.vertices . catAreas

knowledgeItems :: Catalogue -> [Item]
knowledgeItems = G.vertices . catAreas

knowledgeAreas :: Catalogue -> [Item]
knowledgeAreas = filter ((==KA) . itemType) . knowledgeItems

knowledgeUnits :: Catalogue -> [Item]
knowledgeUnits = filter ((==KU) . itemType) . knowledgeItems

knowledgeTopics :: Catalogue -> [Item]
knowledgeTopics = filter ((==KT) . itemType) . knowledgeItems

-- TODO: generalize so that it works with topics and areas...
addArea :: Catalogue -> ItemTree -> ItemTree
addArea c n@(Node x ns) = case getItemArea c (itemId x) of
  Nothing -> n
  Just x' -> Node x' [n]

getItemArea :: Catalogue -> ItemId -> Maybe Item
getItemArea c x = getItem c $ x { unitId = Nothing, topicId = Nothing }

itemLinks :: Catalogue -> Item -> [(Link,ItemId)]
itemLinks c x = map (\(l,x) -> (l,itemId x)) . G.outEdges x $ catAreas c

itemLinks' :: Catalogue -> ItemId -> Maybe [(Link,ItemId)]
itemLinks' c x = itemLinks c <$> getItem c x

-- changes item ids, inclusive in links
changeItemIds :: Catalogue -> [(ItemId ,ItemId)] -> Catalogue
changeItemIds c xs = c 
  { catAreas = G.fromEdgeList . map (\(x1,l,x2) -> (f x1,l,f x2)) . 
               G.toEdgeList $ catAreas c }
  where f x = let x' = itemId x in x { itemId = fromMaybe x' $ lookup x' xs }

itemIdCorrections :: Catalogue -> [(ItemId,ItemId)]
itemIdCorrections c = concatMap (fix0 . fmap itemId) $ catalogueForest c
  where fix0 (Node n ns) = concat $ zipWith (fix n) ns [1..]
        fix y (Node x ns) i
          | x == childId y i = concat $ zipWith (fix x) ns [1..]
          | otherwise        = (x,childId y i) : (concat $ zipWith (fix x) ns [1..])

fixItemIds :: Catalogue -> Catalogue
fixItemIds cat = changeItemIds cat $ itemIdCorrections cat

childId :: ItemId -> Int -> ItemId
childId x i = case (unitId x,topicId x) of
  (Nothing, Nothing) -> x { unitId = Just i }
  (_,       Nothing) -> x { topicId = Just i }

-- make faster
nonuniqueIds :: Catalogue -> [ItemId]
nonuniqueIds c = xs \\ nub xs
  where xs = map itemId $ knowledgeItems c

------------------------------------------------------------------------------
-- Catalogue--ItemTree conversions
------------------------------------------------------------------------------

type ItemTree = Tree Item

{-
rootConnect :: [ItemTree] -> ItemTree
rootConnect ns = Node rootNode ns
-}

catalogueFromForest :: [ItemTree] -> Catalogue
catalogueFromForest forest = 
  emptyCat { catAreas = G.unions $ map (G.fromTree (const SubItem)) forest }

catalogueForest :: Catalogue -> [ItemTree]
catalogueForest cat = 
  --sortBy (compare `on` (itemId . rootLabel)) .
  map (\x -> G.toTree x (==SubItem) g) $ knowledgeAreas cat
  where g = catAreas cat

-- get item tree rooted in item x
getItemTree :: Catalogue -> ItemId -> Maybe ItemTree
getItemTree c x = (\x -> G.toTree x (==SubItem) (catAreas c)) <$> getItem c x

-- gets tree that holds items
treesWithItems :: Catalogue -> [ItemId] -> [ItemTree]
treesWithItems c xs = 
  map (filterTree2 (\x -> itemId x `elem` xs)) ts
  where ts  = mapMaybe (getItemTree c) . sort . nub $
              map (\x -> x {unitId = Nothing, topicId = Nothing}) xs

addItemTree :: Catalogue -> ItemTree -> Catalogue
addItemTree c t =
  c { catAreas = G.union (catAreas c) $ G.fromTree (const SubItem) t }

-- fixes itemids but discards all links but subitem links (suitable for components)
fixTreeItemIds :: ItemTree -> ItemTree
fixTreeItemIds = fix0
  where fix0 (Node x ns) = Node x $ zipWith (fix x) ns [1..]
        fix y (Node x ns) i 
          | itemId x == childId (itemId y) i = 
            Node x $ zipWith (fix x) ns [1..]
          | otherwise = 
            Node (x { itemId = childId (itemId y) i }) $ zipWith (fix x) ns [1..]

------------------------------------------------------------------------------
-- Catalogue output to CSV
------------------------------------------------------------------------------

csvCatalogueSubset :: Catalogue -> [ItemId] -> CSV
csvCatalogueSubset c = concatMap (csvItemTree c) . treesWithItems c

csvItemTree :: Catalogue -> ItemTree -> CSV
csvItemTree c (Node x ns) = csvItem c x : concatMap (csvItemTree c) ns
 
csvItem :: Catalogue -> Item -> Row
csvItem c x
  | itemType x == KA =
      [showItemId $ itemId x,itemLabel x,"","","","",
       fromMaybe "" $ itemRemark x,showItemEditors x,overlaps]
  | itemType x == KU =
      ["","",showItemId $ itemId x,itemLabel x,"","",
       fromMaybe "" $ itemRemark x,showItemEditors x,overlaps]
  | itemType x == KT =
      ["","","","",showItemId $ itemId x,itemLabel x,
       fromMaybe "" $ itemRemark x,showItemEditors x,overlaps]
  where overlaps = intercalate ", " $ 
                   map (showItemId . snd) . filter ((==Overlaps) . fst) $ itemLinks c x

showItemEditors :: Item -> String
showItemEditors = intercalate ", " . itemEditors
  
header =
  [ "KA Id","KA Name","KU Id","KU Name","KT Id","KT Name","Remark"
  , "Editors","Overlaps" ]

csvCatalogue :: Catalogue -> CSV
csvCatalogue c = 
  ["FER3 Knowledge Catalogue"] :
  ["Catalogue", catName c] :
  ["Catalogue ID", catId c] :
  ["Version", fromMaybe "" $ catVersion c] :
  ["Date", fromMaybe "" $ catDate c] :
  ["Editors", intercalate ", " $ catEditors c] :
  ["Remark", fromMaybe "" $ catRemarks c] : [] :
  header : [] :
  (concatMap (csvItemTree c) $ catalogueForest c)

saveCatalogue :: FilePath -> Catalogue -> IO ()
saveCatalogue f = writeFile f . showCSV . csvCatalogue 

addItemRemark :: Catalogue -> ItemId -> String -> Catalogue
addItemRemark c x r = modifyItem c x addRemark
  where addRemark x = x  
          { itemRemark = case itemRemark x of
              Nothing -> Just r 
              Just r' -> Just $ r ++ "; " ++ r' }

filterTree :: Eq a => (a -> Bool) -> Tree a -> Tree a
filterTree p (Node x ns) = 
  Node x . map (filterTree p) $ filter (any p . flatten) ns

-- includes hanging substrees
filterTree2 :: Eq a => (a -> Bool) -> Tree a -> Tree a
filterTree2 p n@(Node x ns) 
  | p x       = n
  | otherwise = Node x . map (filterTree2 p) $ filter (any p . flatten) ns


------------------------------------------------------------------------------
-- TMP

main = do
  Right (c,ls) <- loadCatalogueComponent "../data/catalogue/v0.2/components-resolved-csv/g020-c022.res.csv"
  printCSV $ csvCatalogue c
  print $ map (\(x1,x2,l) -> (showItemId x1, showItemId x2, l)) ls

main2 = do
  Right c <- loadCatalogue "../data/catalogue/v0.2/catalogue/FER3-v0.2.3.csv"
  printCSV $ csvCatalogue c

correct = do 
  Right c <- loadCatalogue "../data/catalogue/v0.2/catalogue/FER3-v0.2.3.csv"
  let xs = itemIdCorrections c
  saveCatalogue "FER3-v0.2.3.csv" $ changeItemIds c xs  

{-

Right c <- loadCatalogue "../data/catalogue/v0.2/catalogue/FER3-v0.2.2.csv" 
printCSV $ csvCatalogue c

Right c <- loadCatalogue "../data/catalogue/v0.2/components-resolved-csv/g020-c022.res.csv"
printCSV $ csvCatalogue c

let t = catalogueForest c
putStr $ showCSV $ concatMap (csvItemTree c) t

let Just t =  getItemTree c (fromJust $ readItemId "CS-IS")
putStr $ showCSV $ csvItemTree c t

-}

