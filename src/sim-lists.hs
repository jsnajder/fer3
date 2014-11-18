--import Catalogue
import Control.Applicative ((<$>))
import Reader.CSV
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.Parsec hiding (label,labels)
import Text.Parsec.String
import Text.Printf
import qualified LabeledGraph as G

data SimLabel = O | X | E | R deriving (Eq,Show,Read,Ord)
type ItemId = String
type SimGraph = G.Graph ItemId ItemId (Maybe SimLabel)
type SimGraphPaired = G.Graph ItemId ItemId (Maybe SimLabel, Maybe SimLabel)

pairSimGraph :: SimGraph -> SimGraphPaired
pairSimGraph = undefined

readSimGraph :: String -> SimGraph
readSimGraph = G.fromAdjacencyList . readSimList

readSimList :: String -> [(ItemId, [(Maybe SimLabel, ItemId)])]
readSimList s = case parse parseSimList "(unknown)" s of
  Right c -> c
  Left  e -> error $ show e

parseSimList :: Parser [(ItemId, [(Maybe SimLabel,ItemId)])]
parseSimList = parseItemList `sepBy1` (many1 (char ',') >> newline)
 
parseItemList :: Parser (ItemId, [(Maybe SimLabel,ItemId)])
parseItemList = do
  itemId <- parseItemId
  nextLine
  items <- many1 (try parseLabeledItem)
  return (itemId,items)

parseItemId :: Parser ItemId
parseItemId = do
  char '['
  itemId <- many1 (noneOf "]")
  char ']'
  return itemId

parseLabeledItem :: Parser (Maybe SimLabel,ItemId)
parseLabeledItem = do
  simLabel <- parseSimLabel
  anyChar `manyTill` char ','
  itemId <- parseItemId
  nextLine
  return (simLabel,itemId)

parseSimLabel :: Parser (Maybe SimLabel)
parseSimLabel = do
  label <- many (noneOf ",")
  char ','
  let ambigLabels = map (read . (:[])) . filter (`elem` "XROE") $ map toUpper label
  return $ disambigLabel ambigLabels

disambigLabel :: [SimLabel] -> Maybe SimLabel
disambigLabel []  = Nothing
disambigLabel [l] = Just l
disambigLabel ls | E `elem` ls = Just E
                 | O `elem` ls = Just O
                 | R `elem` ls = Just R
                 | otherwise   = Just X

{-
-- todo: find missmatches

showSimCatalogue :: SimCatalogue -> String
showSimCatalogue = unlines . map showItemList . M.toList
   where showItemList (item,labeledItems) = unlines $ 
           showItem item : map showLabeledItem labeledItems
         showItem item = printf "[%s],%s,,,,%s"
           (itemId item) (csvQuote $ itemLabel item)
           (csvQuote . intercalate ", " $ itemEditors item)
         showLabeledItem (label,item) = printf "%s,%s,[%s],%s,%s"
           (show label) "" (itemId item) (csvQuote $ itemLabel item)
           (csvQuote . intercalate ", " $ itemEditors item)

showSimCataloguePaired :: SimCataloguePaired -> String
showSimCataloguePaired = unlines . map showItemList . M.toList
   where showItemList (item,labeledItems) = unlines $ 
           showItem item : map showLabeledItem labeledItems
         showItem item = printf "[%s],%s,,,,%s"
           (itemId item) (csvQuote $ itemLabel item)
           (csvQuote . intercalate ", " $ itemEditors item)
         showLabeledItem ((label1,label2),item) = printf "%s,%s,[%s],%s,%s"
           (show label1) 
           (if isJust label2 then show (fromJust label2) else "") 
           (itemId item) (csvQuote $ itemLabel item)
           (csvQuote . intercalate ", " $ itemEditors item)
        
getSimLabel :: SimCatalogue -> ItemId -> ItemId -> Maybe SimLabel
getSimLabel c id1 id2 = do
  item1 <- getVertex ((==id1) . itemId) c
  item2 <- getVertex ((==id2) . itemId) c
  getEdgeLabel c item1 item2

main = do
  f1 <- readFile "../../data/catalogue/v0.1/sim-lists/FER3-IP.sim.csv"
  f2 <- readFile "../../data/catalogue/v0.1/sim-lists/FER3-MA.sim.csv"
  let c1 = readSimCatalogue f1
      c2 = readSimCatalogue f2
      c3 = pairSimCatalogues [c1,c2]
  writeFile "tmp.csv" $ showSimCataloguePaired c3


type SimCataloguePaired = LabeledGraph Item (SimLabel,Maybe SimLabel)

pairSimCatalogues :: [SimCatalogue] -> SimCataloguePaired
pairSimCatalogues cs = M.fromList . map addLabels $ M.toList c
  where addLabels (item1,links) = (item1, [((l,l2),item2)
          | (l,item2) <- links
          , let l2 = getSimLabel c (itemId item2) (itemId item1) ])
        c = M.unions cs


readSimCatalogue :: String -> SimCatalogue
readSimCatalogue s = case parse parseSimCatalogue "(unknown)" s of
  Right c -> c
  Left  e -> error $ show e

parseSimCatalogue :: Parser SimCatalogue
parseSimCatalogue = do
  xs <- parseItemList `sepBy1` emptyRow
  return $ M.fromList xs

parseItemList :: Parser (Item, [(SimLabel,Item)])
parseItemList = do

parseItemId :: Parser ItemId
parseItemId = do
  itemId <- stringCell
  return . tail $ init itemId

parseItemEditors :: Parser [ItemEditor]
parseItemEditors = 
  stringSequence <$> stringCell

parseItem :: Parser Item
parseItem = do
  itemId <- parseItemId
  itemLabel <- stringCell
  count 3 (char ',')
  itemEditors <- parseItemEditors
  return $ Item { itemId = itemId, itemType = KU, itemLabel = itemLabel
                , itemVersion = Nothing
                , itemEditors = itemEditors
                , itemRemark  = Nothing }
-}
