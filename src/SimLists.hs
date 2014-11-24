{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module SimLists where

import Catalogue
import Control.Applicative ((<$>), liftA2)
import Control.Monad
import CSV (readCSV, Field)
import Data.Char
import qualified Data.EdgeLabeledGraph as G
import Data.Function
import Data.List
import Data.List.Partition (eqClassesGen)
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

data SimLabel = X | R | O | E deriving (Eq,Show,Read,Ord)
type ItemKey = T.Text
type SimGraph = G.Graph ItemKey ItemId SimLabel
type SimGraphPaired = G.Graph ItemKey ItemId (SimLabel, SimLabel)

instance G.Vertex ItemId T.Text where
  index = T.pack . showItemId

dir = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.1/sim-lists"

diagreements = do
  fs <- csvFiles dir
  gs <- loadSimLists fs
  return $ disagreementGraph gs

overlaps = do
  fs <- csvFiles dir
  gs <- loadSimLists fs
  return . overlapGraph . pairGraph $ G.unions gs

overlapingUnits g = 
 eqClassesGen (\v -> map snd $ G.outEdges v g) (G.vertices g)

csvFiles d = 
  map (d </>) . filter (".sim.csv" `isSuffixOf`) <$> getDirectoryContents d

loadSimLists :: [FilePath] -> IO [SimGraph]
loadSimLists fs = do
  forM fs $ \f -> do
    putStrLn f
    Right g <- readSimGraphCSV <$> readFile f
    g `seq` return g

disagreementGraph :: [SimGraph] -> SimGraphPaired
disagreementGraph = G.filterEdges dis . pairGraph . G.unions
  where dis _ _ (l1,l2) = l1 /= l2 && 
                          (not ((l1 == E && l2 == O) || (l1 == O && l2 == E)))
{-
partitionByCat :: SimGraphPaired -> [(String,SimGraphPaired)]
partitionByCat g = 
  [ (edgeOutCat e, G.fromEdgeList es) | 
    es@(e:_) <- groupBy ((==) `on` edgeOutCat) $ G.toEdgeList g ]
  where edgeOutCat (v1,_,_) = catCode v1
-}

pairGraph :: 
  (G.Vertex v k, Eq v, Ord k, Eq l) => G.Graph k v l -> G.Graph k v (l,l)
pairGraph g = G.fromEdgeList $ do
  v1       <- G.vertices g
  (l1,v2)  <- G.outEdges v1 g
  (l2,v1') <- G.outEdges v2 g
  guard $ v1==v1'
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

readItemId' = readItemId . init . tail

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

------------------------------------------------------------------------------

type OverlapGraph = G.Graph ItemKey ItemId ()

overlapGraph :: SimGraphPaired -> OverlapGraph
overlapGraph = G.fromEdgeList . mapMaybe f . G.toEdgeList
  where f (v1,(l1,l2),v2) 
          | l1 `elem` [O,E] || l2 `elem` [O,E] = Just (v1,(),v2)
          | otherwise = Nothing
--TODO: enforce hierachical constraints!
--TODO: fix CS-CE-SE spurious overlaps

type OverlapCatalogue = G.Graph ItemKey Item ()

overlapCatalogue :: OverlapGraph -> Catalogue -> OverlapCatalogue
overlapCatalogue og c = undefined

