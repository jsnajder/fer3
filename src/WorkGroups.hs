
module WorkGroups where

import Catalogue
import Control.Applicative
import Control.Monad hiding (ap)
import CSV
import qualified Data.Counts as C
import Data.Function
import Data.List
import qualified Data.EdgeLabeledGraph as G
import Data.Graph.Inductive 
import qualified Data.Map as M
import Data.Maybe
import Data.List.Partition (eqClasses)
import SimLists
import qualified SplitGraph as SG
import System.FilePath
import Text.Printf

type OverlapGraph = Gr ItemId ()

mkOverlapGraph :: SimGraphPaired -> OverlapGraph
mkOverlapGraph g = undir $ mkGraph (zip [0..] nodes) edges
  where nodes = filter isUnit . nub . map lift $ G.vertices g
        edges = do
                  (v1,(l1,l2),v2) <- G.toEdgeList g
                  let (v1',v2') = (lift v1, lift v2)
                  guard $ isUnit v1' && isUnit v2'
                  guard $ l1 `elem` [O,E] || l2 `elem` [O,E]
                  let Just i1 = M.lookup v1' ix
                  let Just i2 = M.lookup v2' ix
                  return (i1,i2,())
        ix = M.fromList $ zip nodes [0..]
        isUnit x = (isJust . unitId $ x) && (topicId x == Nothing)
        lift x = x { topicId = Nothing } 

g = mkGraph (zip [1..5] "12345") 
            [(1,2,"12"),(2,3,"23"),(1,3,"13"),(3,4,"34"),(3,5,"35"),(4,5,"45")] :: Gr Char String

g2 = mkGraph (zip [1..6] "123456") 
            [(1,2,"12"),(2,3,"23"),(1,3,"13"),(6,4,"34"),(6,5,"35"),(4,5,"45")] :: Gr Char String

g3 = mkGraph (zip [1..6] "123456") 
            [(1,2,"12"),(2,3,"23"),(1,3,"13"),(3,4,"34"),(3,5,"35"),(4,5,"45"),(4,6,"46")] :: Gr Char String

{-
sccGraphs :: Graph gr => gr a b => [gr a b]
sccGraphs g = [ delNodes ys g | xs <- scc g, let ys = ns \\ xs ]
  where ns = nodes g
-}

loadOverlapGraph = do
  Right c <- loadCatalogue catFile
  fs <- csvFiles simDir
  g  <- addCompEdges c . filterSpurious <$> loadSimLists fs
  let og = mkOverlapGraph . pairGraph2 $ g
  return $ og

addOverlapLinks :: OverlapGraph -> Catalogue -> Catalogue
addOverlapLinks og c = c { catAreas = g' }
  where g  = catAreas c
        es = mapMaybe oEdge $ edges og
        g' = g `G.union` G.fromEdgeList es
        oEdge (x, y) = case (lab og x >>= getItem c, lab og y >>= getItem c) of
                         (Just x,Just y) -> Just (x,Overlaps,y)
                         _               -> Nothing

type OverlapComponent = (Int, [(ItemId, [Int])])

maxComponentSize = 10

overlapComponents :: OverlapGraph -> [OverlapComponent]
overlapComponents og = 
  [ (i, map (mkUnit i) $ nodes oc) | (i,oc) <- ocs ]
  where ocs  = zip [1..] . concatMap (SG.splitToSize maxComponentSize) . 
               filter ((>1) . noNodes) $ SG.gComponents og
        gi x = map fst $ filter ((x `gelem`) . snd) ocs
        mkUnit i x = (fromJust $ lab og x, 
                      filter (/=i) $ gi x)

analyseOverlaps = do
  og <- loadOverlapGraph
  Right cat <- loadCatalogue catFile
  let  oc  = filter ((>1) . noNodes) $ SG.gComponents og
       n   = sum $ map noNodes oc
       xs  = C.fromList $ map noNodes oc
       oc' = concatMap (SG.splitToSize maxComponentSize) oc
       n'  = sum $ map noNodes oc'
       xs' = C.fromList $ map noNodes oc'
       pl  = C.fromList $ map longestShortestPath oc'
       a   = length . nub $ concatMap nodes oc' \\ concatMap nodes oc
       zs  = C.fromList . map snd . C.counts . C.fromList $ concatMap nodes oc'
       gs  = mkGroups cat $ overlapComponents og
       ges = C.fromList $ map (length . nub . concatMap (componentEditors cat)) gs
       gcs = C.fromList $ map length gs
  putStr . unlines $
    [ printf "%d non-singleton components" (length oc) 
    , printf "%d nodes in non-singleton components" n 
    , printf "components size histogram: %s" (show $ C.counts xs) 
    , printf "size-%d-splitted components: %d" maxComponentSize (length oc')
    , printf "components size histogram: %s" (show $ C.counts xs')
    , printf "longest shortest paths histogram: %s" (show $ C.counts pl)
    , printf "articulation points split: %d" a
    , printf "articulation points multiplicity histogram: %s" (show $ C.counts zs)
    , printf "number of groups: %d" (length gs)
    , printf "group sizes (by components) histogram: %s" (show $ C.counts gcs) 
    , printf "group sizes (by editors) histogram: %s" (show $ C.counts ges) ]

longestShortestPath :: Gr a b -> Int
longestShortestPath g = maximum $ map (longest g) (nodes g)
  where longest g x = maximum . map (\(LP p) -> length p) . spTree x $ emap (const 1) g

{-
mkGroups :: Catalogue -> [OverlapComponent] -> [([OverlapComponent], [ItemEditor])]
mkGroups cat oc = 
  [ (map fst xs, e) | xs@((x,e):_) <- eqClasses ((==) `on` snd) xss ]
  where xss = map (\x -> (x, componentEditors cat x)) oc
-}

mkGroups :: Catalogue -> [OverlapComponent] -> [[OverlapComponent]]
mkGroups cat oc = 
  [ map fst xs | xs@((x,e):_) <- eqClasses ((==) `on` snd) xss ]
  where xss = map (\x -> (x, componentCats cat x)) oc

componentCats :: Catalogue -> OverlapComponent -> [String]
componentCats c (_,zs) =
  nub . sort $ map (\(x,_) -> catCode x) zs

componentEditors :: Catalogue -> OverlapComponent -> [ItemEditor]
componentEditors cat (_,zs) =
  nub . sort $ concatMap (\(x,_) -> let i = getItem cat x
                                    in if isNothing i then error ("cannot find " ++ show x) 
                                       else itemEditors . fromJust $ getItem cat x) zs

{-
csvOverlapComponent :: Catalogue -> OverlapComponent -> CSV 
csvOverlapComponent c oc@(i,zs) = 
  [printf "COMPONENT %d" i] : [printf "EDITORS: %s" editors] : 
  [printf "This component has %d KUs" $ length zs] : [w] : [] :
  concatMap csvUnit zs
  where csvUnit (x,o,z) = 
          let r1 = intercalate ", " $ map showItemId o
              r2 = printf "SHARED WITH COMPONENTS: %s / Overlaps with KUs: %s" 
                      (intercalate ", " $ map show z)
          in (++[[]]) . addRemark r2 . addRemark r1 . fromJust $ csvKnowledgeUnit c x
        editors = intercalate ", " $ componentEditors c oc
        hasRepeated = any (\(_,_,xs) -> not $ null xs) zs
        w = if hasRepeated then "WARNING: This component shares some KUs with other components (see remarks in column J)" else ""
-}      

csvOverlapComponent :: Catalogue -> OverlapComponent -> CSV 
csvOverlapComponent c oc@(i,zs) = 
  [printf "COMPONENT %d (contains %d KUs)" i (length zs)] : 
  [printf "SUBCATS (%d): %s" (length as) (intercalate ", " as)] :
  [printf "EDITORS (%d): %s" (length editors) (intercalate ", " editors)] : [w] : [] : 
  ["KA Id","KA Name","KU Id","KU Name","KT Id","KT Name","Comment","Editors","Overlaps"] :  [] :
  csvCatalogueSubset c2 units
  where units = map (\(x,_) -> x) zs
        editors = componentEditors c oc
        as = componentCats c oc
        hasRepeated = any (\(_,xs) -> not $ null xs) zs
        w = if hasRepeated then "WARNING: This component shares some KUs with other components (see remarks in column G)" else ""
        r z = if null z then "" else
                printf "SHARED WITH COMPONENTS: %s" (intercalate ", " $ map show z)
        c2 = foldl' (\c (x,z) -> addItemRemark c x (r z)) c zs

componentsDir = dir </> "components"

main = do
  og <- loadOverlapGraph
  Right c' <- loadCatalogue catFile
  let c   = addOverlapLinks og c'
      ocs = overlapComponents og
      rest = map itemId (knowledgeUnits c) \\ concatMap (map fst . snd) ocs
      restComp = (0, map (\x -> (x,[])) rest)
      gs  = [restComp] : mkGroups c ocs
  forM_ (zip [(0::Int)..] gs) $ \(i,ocs) -> do
    forM_ ocs $ \oc@(j,_) -> do 
      let csv = csvOverlapComponent c oc
      writeFile (componentsDir </> printf "g%03d-c%03d.csv" i j) (showCSV csv)

