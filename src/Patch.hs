
module Patch where

import Catalogue
import Control.Applicative
import Control.Monad
import CSV
import qualified Data.EdgeLabeledGraph as G  -- TMP!
import Data.List
import Data.Maybe
import qualified Data.Set as S
import System.Directory
import System.Environment
import System.FilePath
import Data.Tree

import Debug.Trace

type Patch = (Catalogue, [(ItemId,Op)])

data Op = ReplaceBy [ItemId] | Delete | Resolve | Modify | Add
  deriving (Eq,Ord,Show,Read)

readPatch :: String -> Either ParseError Patch
readPatch s = mk . readItemForest . dropWhile isHeader <$> readCSV s
  where mk ts = ( emptyCat { catAreas = G.unions $ 
                    map (G.fromTree (const SubItem) . fixTreeTopicIds . fmap fst) ts }
                , mapMaybe readOp . filter ((==KU) . itemType . fst) $ concatMap flatten ts )
        isHeader [] = True
        isHeader (x:_) = isNothing $ readItemId' x

readOp :: (Item,[Field]) -> Maybe (ItemId,Op)
readOp (x, zs) = case zs !!! 1 of
  Nothing -> Nothing
  Just "DEL" -> Just (x', Delete)
  Just "ADD" -> Just (x', Add)
  Just xs -> Just $ (x', let ys = readItemIds xs
                         in if ys==[itemId x] then Resolve else ReplaceBy ys)
  where x' = itemId x

loadPatch :: FilePath -> IO (Either ParseError Patch)
loadPatch f = readPatch <$> readFile f

data Diff = Diff
  { removed  :: [ItemId]
  , added    :: [ItemId]
  , modified :: [ItemId]
  , resolved :: [ItemId]
  , replaced :: [(ItemId,ItemId)] } deriving (Eq,Ord,Show)

patchDiff :: Catalogue -> Patch -> Diff
patchDiff cat (cmp,ops) = Diff
  { removed  = removed
  , added    = added
  , modified = modified
  , resolved = resolved
  , replaced = replaced }
  where catUnits  = knowledgeUnits cat
        cmpUnits  = knowledgeUnits cmp
        catUnits' = map itemId catUnits
        cmpUnits' = map itemId cmpUnits
        removed   = [x | (x,Delete) <- ops, x `elem` catUnits']
        add       = [x | (x,Add) <- ops]
        replaced  = [(x,y) | (x,ReplaceBy ys) <- ops, x `elem` catUnits', y <- ys]
        replaceTo = nub $ map snd replaced
        resolved  = [x | (x,Resolve) <- ops]
        -- add a unit if (1) pointed to or (2) not changed, but new
        added     = (nub $ replaceTo ++ add) \\ catUnits'
        modified  = filter (not . fromJust . identicalItems cat cmp) $
                    ((cmpUnits' `intersect` catUnits') \\ removed) \\ added

identicalItems :: Catalogue -> Catalogue -> ItemId -> Maybe Bool
identicalItems c1 c2 x = liftA2 (==) (getItemTree c1 x) (getItemTree c2 x)
--  where f = fmap (\x -> x { itemRemark = Nothing, 
--                            itemEditors = if itemType x == KT then [] 
--                                            else itemEditors x  })

patch :: Catalogue -> Patch -> (Catalogue, Diff)
patch c p@(cmp,_) = (c6, d)
  where d  = patchDiff c p
        c2 = removeItems' c $ removed d
        adds = map fixTreeTopicIds . treesWithItems cmp $ added d ++ modified d
        c3 = foldl' addItemTree c2 adds
        overlaps = overlapLinks c3
        resolved' = resolved d ++ map fst (replaced d)
        units = map itemId $ knowledgeUnits cmp
        overlaps2 = filter (\(x1,x2,_) -> 
                      (x1 `elem` resolved' && x2 `elem` units) ||
                      (x1 `elem` units && x2 `elem` resolved')) overlaps
        c4 = foldl' removeLink c3 overlaps2
        c5 = combineEditorsAndOverlaps c4 p
        c6 = foldl' (\c (x1,x2) -> fromMaybe c (replaceItem' c x1 x2)) c5 $ replaced d

overlapLinks :: Catalogue -> [(ItemId,ItemId,Link)]
overlapLinks = filter (\(_,_,l) -> l==Overlaps) . links'

-- for all ReplaceBy targets, take ReplaceBy source editors
-- for all ReokaceBy targets, take ReplaceBy 
-- (remark: inefficient, because diff is computed again)
combineEditorsAndOverlaps :: Catalogue -> Patch -> Catalogue
combineEditorsAndOverlaps c p@(cmp,_) = c3
  where Diff rem add mod ret rep = patchDiff c p
        rep' = mapMaybe (\(x1,x2) -> 
                 liftA2 (,) (getItem c x1) (getItem cmp x2)) rep
        addEditors x1 x2 = x2 {
            itemEditors = sort . nub $ itemEditors x1 ++ itemEditors x2 }
        links' = substitute $ overlapLinks c
        c2 = foldl' (\c (x1,x2) -> modifyItem c (itemId x2) (addEditors x1)) c rep'
        c3 = fromJust $ addLinks c2 links'
        substitute xs = [(x1',x2',l) | (x1,x2,l) <- xs,
                         let x1' = fromMaybe x1 $ lookup x1 rep,
                         let x2' = fromMaybe x2 $ lookup x2 rep, 
                         x1'/=x2', 
                         x1 `elem` map fst rep || x2 `elem` map fst rep ]

-- patch is bogus if it operates on a (non-added) unit that does not exist in the
-- catalogue or maps to a unit that does not exist in the component nor in the 
-- catalogue
bogusPatch :: Catalogue -> Patch -> Bool
bogusPatch c p@(cmp,ops) = 
  (opItems   `intersect` catUnits /= opItems) ||
  (replaceTo `intersect` (catUnits ++ cmpUnits) /= replaceTo) 
  where catUnits  = map itemId $ knowledgeUnits c
        cmpUnits  = map itemId $ knowledgeUnits cmp
        opItems   = map fst ops \\ added
        replaceTo = nub $ [y | (_,ReplaceBy ys) <- ops, y <- ys]
        added     = [x | (x,Add) <- ops]

patchAndLog :: Catalogue -> FilePath -> IO (Catalogue,CSV)
patchAndLog c f = do
  Right p <- loadPatch f
  let (c2,d) = patch c p
      Diff rem add mod res rep = d
      xs = [[patchFile,showItemId x,"Removed"] | x <- rem] ++ 
           [[patchFile,showItemId x,"Added"] | x <- add] ++ 
           [[patchFile,showItemId x,"Modified"] | x <- mod] ++ 
           [[patchFile,showItemId x,"Declared as non-overlapping (within this component)"] | x <- res] ++ 
           [[patchFile,showItemId x,"Replaced by " ++ showItemId y] | (x,y) <- rep]
  return (c2,zipWith (\x xs -> show x : xs) [1..] xs)
  where patchFile = takeFileName f

dir = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.3"
catFile = dir </> "FER3-KC-v0.3.0.csv"
dirInbox = dir </> "components-csv-resolved"
dirOk = dirInbox </> "ok"
dirBogus = dirInbox </> "bogus"
newCatFile = "FER3-KC-v0.4.0.csv"
dirOut = dir </> "resolved"

sortPatches = do
  Right c <- loadCatalogue catFile
  fs <- filter (".csv" `isSuffixOf`) <$> getDirectoryContents dirInbox
  forM_ fs $ \f -> do
    putStr $ f ++ ": "
    Right p <- loadPatch $ dirInbox </> f
    if bogusPatch c p then do
        putStrLn "BOGUS"
        renameFile (dirInbox </> f) (dirBogus </> f)
      else do
        putStrLn "OK"
        renameFile (dirInbox </> f) (dirOk </> f)

applyPatches = do
  Right c <- loadCatalogue catFile
  fs <- map (dirOk </>) . filter (".csv" `isSuffixOf`) <$> getDirectoryContents dirOk
  (cNew,log) <- foldM (\(c,csv) f -> do
    (c2,csv2) <- patchAndLog c f
    putStrLn $ f ++ show (length csv2)
    return (c2,csv++csv2)) (c,[]) fs
  saveCatalogue (dirOut </> newCatFile) (removeTopicEditors $ addInfoRemark cNew)
  writeFile (dirOut </> "log.csv") (showCSV log)
 
