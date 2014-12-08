{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module SimLists where

import Catalogue
import Control.Applicative ((<$>), liftA2)
import Control.Monad
import CSV (readCSV, showCSV, Field, CSV)
import Data.Char
import qualified Data.EdgeLabeledGraph as G
import Data.Function
import Data.List
import Data.List.Partition (eqClassesGenOut)
import Data.List.Split (splitWhen)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath
import Text.Parsec hiding (label,labels)
import Text.Parsec.String
import Text.Printf
import Debug.Trace

data SimLabel = X | R | O | E deriving (Eq,Show,Read,Ord)
type ItemKey = String
type SimGraph = G.Graph ItemKey ItemId SimLabel
type SimGraphPaired = G.Graph ItemKey ItemId (SimLabel, SimLabel)

instance G.Vertex ItemId ItemKey where
  index = showItemId

dir     = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.2/"
simDir  = dir </> "sim-lists-corr"
catFile = dir </> "catalogue/FER3-v0.2.csv"
outDir  = dir </> "sim-lists-disagree/csv"

csvFiles d = 
  map (d </>) . filter (".csv" `isSuffixOf`) <$> getDirectoryContents d

loadSimLists :: [FilePath] -> IO SimGraph
loadSimLists fs = G.unions <$> do
  forM fs $ \f -> do
    putStrLn f
    Right g <- readSimGraphCSV <$> readFile f
    g `seq` return g

generateDisagreementLists :: IO ()
generateDisagreementLists = do
  Right c <- loadCatalogue catFile
  fs <- csvFiles simDir
  g  <- filterSpurious <$> loadSimLists fs
  let d = disagreementGraph g
  forM_ (splitSimGraph d) $ \(cat,d) -> do
    let cvs = csvSimList d c
        fn  = printf "FER3-%s.sim.disagree.csv" cat
    writeFile (outDir </> fn) $ showCSV cvs

-- filters out some spurious labelings
filterSpurious :: SimGraph -> SimGraph
filterSpurious = G.filterEdges (\v1 v2 _ -> not $ spuriousMatch v1 v2)
  
spuriousMatch x1 x2 = 
  catCode x1 `elem` ["CS","CE","SE"] &&
  catCode x2 `elem` ["CS","CE","SE"] &&
  catCode x1 /= catCode x2 &&
  areaCode x1 == areaCode x2 &&
  unitId x1 /= unitId x2

-- add CS-CE-SE edges
addCompEdges :: SimGraph -> SimGraph
addCompEdges g = g `G.union` G.fromEdgeList es
  where p x = catCode x `elem` ["CS","CE","SE"] && isJust (unitId x) && isNothing (topicId x)
        es = [(x,E,y)  | x <- G.vertices g, p x, y <- G.findVertex (matches x) g ]
        matches x y = catCode y `elem` ["CS","CE","SE"] && catCode x /= catCode y &&
                      areaCode x == areaCode y && unitId x == unitId y &&
                      isNothing (topicId y)

disagreementGraph :: SimGraph -> SimGraphPaired
disagreementGraph = G.filterEdges dis . pairGraph2
  where dis _ _ (l1,l2) = l1 /= l2 && 
                          (not ((l1 == E && l2 == O) || (l1 == O && l2 == E)))
--                                (l1 == X && l2 == R) || (l1 == R && l2 == X)))

csvSimList :: SimGraphPaired -> Catalogue -> CSV
csvSimList sg c = concatMap (csvItemList sg c) . G.vertices $ catAreas c

csvItemList :: SimGraphPaired -> Catalogue -> Item -> CSV
csvItemList sg c x 
  | null xs = []
  | otherwise = 
      [showItemId $ itemId x, itemLabel x, [], [], [], showEditors x] :
      map csvLabeledItem xs ++ [[]]
  where xs = simItems sg c x
        csvLabeledItem (l1,l2,x) =
          [show l1, show l2, showItemId $ itemId x, itemLabel x, showEditors x]
        showEditors = intercalate ", " . itemEditors

splitSimGraph :: SimGraphPaired -> [(String, SimGraphPaired)]
splitSimGraph =
  map (\es@(e:_) -> (outCatCode e, G.fromEdgeList es)) . 
  groupBy ((==) `on` outCatCode) . 
  sortBy (compare `on` outCatCode) . 
  G.toEdgeList 
  where outCatCode (x,_,_) = catCode x

simItems :: 
  SimGraphPaired -> Catalogue -> Item -> [(SimLabel, SimLabel, Item)]
simItems sg cat x =
  mapMaybe (\((l1,l2),id) -> (l1,l2,) <$> getItem cat id) $ 
  G.outEdges (itemId x) sg

pairGraph :: 
  (G.Vertex v k, Eq v, Ord k, Eq l) => G.Graph k v l -> G.Graph k v (l,l)
pairGraph g = G.fromEdgeList $ do
  v1       <- G.vertices g
  (l1,v2)  <- G.outEdges v1 g
  (l2,v1') <- G.outEdges v2 g
  guard $ v1==v1'
  return (v1,(l1,l2),v2)

-- if you don't find a complementary edge, insert one with the same label
pairGraph2 :: 
  (G.Vertex v k, Eq v, Ord k, Eq l) => G.Graph k v l -> G.Graph k v (l,l)
pairGraph2 g = G.fromEdgeList $ do
  v1       <- G.vertices g
  (l1,v2)  <- G.outEdges v1 g
  let Just (l2,v1') = find (\(_,v1') -> v1'==v1) (G.outEdges v1 g ++ [(l1,v1)])
  return (v1,(l1,l2),v2)

readSimGraphCSV :: String -> Either ParseError SimGraph
readSimGraphCSV s = G.fromAdjacencyList <$> readSimListCSV s

readSimListCSV
  :: String -> Either ParseError [(ItemId, [(SimLabel, ItemId)])]
readSimListCSV s = 
  mapMaybe readItemList . filter (not . null) . splitWhen null <$> readCSV s

readItemList :: [[Field]] -> Maybe (ItemId, [(SimLabel,ItemId)])
readItemList ((itemId:_):items) 
  | null items' = Nothing
  | otherwise   = (,items') <$> readItemId' itemId
  where readItem (label:_:itemId:_) =
          liftA2 (,) (readSimLabel label) (readItemId' itemId)
        items' = mapMaybe readItem items

readItemId' s | head s == '[' = readItemId . init $ tail s
              | otherwise     = readItemId s


readSimLabel :: String -> Maybe SimLabel
readSimLabel = 
  disambigLabel . map (read . (:[])) . filter (`elem` "XROE") . map toUpper 

disambigLabel :: [SimLabel] -> Maybe SimLabel
disambigLabel []  = Nothing
disambigLabel [l] = Just l
disambigLabel ls | E `elem` ls = Just E
                 | O `elem` ls = Just O
                 | R `elem` ls = Just R
                 | otherwise   = Just X

