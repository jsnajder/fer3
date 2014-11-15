-- 
-- FER3 Knowledge Catalogue Processing
-- (c) 2014 Jan Snajder, FER
--
------------------------------------------------------------------------------

import Control.Applicative ((<$>))
import Control.Monad
import Data.Function
import Data.List.Split (splitOn,splitOneOf)
import Data.Char
import qualified Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord hiding (compare)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tree
import Data.Traversable hiding (forM)
import qualified NLP.Stemmer as Stemmer
import Prelude hiding (sequence,compare)
import System.FilePath
import System.IO
import Text.Parsec hiding (label,labels)
import Text.Parsec.String
import Text.Printf

------------------------------------------------------------------------------

type Catalogue = Tree Item

data ItemType = CAT | KA | KU | KT deriving (Eq,Show,Enum,Read)

data Item = Item
  { itemType    :: ItemType
  , itemId      :: String
  , itemLabel   :: String
  , itemPath    :: [String]
  , itemVersion :: Maybe String
  , itemEditors :: [String]
  , itemRemark  :: Maybe String } deriving (Eq,Show,Read)

------------------------------------------------------------------------------
-- Basic querying
------------------------------------------------------------------------------

catStats :: Catalogue -> (Int,Int,Int)
catStats c = (ka,ku,kt)
  where [ka,ku,kt] = map (\t -> length $ catItems t c) [KA,KU,KT]

catItems :: ItemType -> Catalogue -> [Item]
catItems t = filter ((==t) . itemType) . flatten

catLabels :: Catalogue -> [String]
catLabels = map itemLabel . flatten

catEditors :: Catalogue -> [String]
catEditors = 
  sortBy (comparing (f . words)) . nub . concatMap itemEditors . flatten
  where f (_:w:ws) = w:ws
        f s        = s

-- Return subcatalogues edited by a given editor (excluding their
-- subcatalogues)
catEditedBy :: String -> Catalogue -> [Catalogue]
catEditedBy e c@(Node x ns) 
  | e `elem` itemEditors x = [c]
  | otherwise              = concatMap (catEditedBy e) ns

------------------------------------------------------------------------------
-- CSV input
------------------------------------------------------------------------------

stringCell :: Parser String
stringCell = do
  s <- quotedCell <|> many1 (noneOf ",\n\r")
  char ',' <|> lookAhead newline
  return s

quotedCell :: Parser String
quotedCell = 
  do char '"'
     content <- many quotedChar
     char '"' <?> "quote at end of cell"
     return content

quotedChar :: Parser Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

readCatalogue :: String -> Catalogue
readCatalogue s = case parse catalogue "(unknown)" s of
  Right c -> c
  Left  e -> error $ show e

nextLine :: Parser ()
nextLine = anyChar `manyTill` newline >> return () 

catalogue :: Parser Catalogue
catalogue = do
  count 2 nextLine
  label <- string "Catalogue:,," >> stringCell
  nextLine
  id <- string "Catalogue ID:,," >> stringCell
  nextLine
  version <- string "Version:,," >> stringCell
  count 2 nextLine
  editor <- string "Editor(s):,," >> stringCell
  count 4 nextLine
  xs <- many (item 1)
  eof
  return $ Node (Item CAT id label [label] (Just version)
                (stringSequence editor) Nothing) xs
  
stringSequence :: String -> [String]
stringSequence [] = []
stringSequence s  = map (unwords . words) $ splitOneOf ",;" s

idLabel :: Parser (String,String)
idLabel = do
  id <- stringCell <?> "item identifier"
  label <- try stringCell <|> (char ',' >> return "") <?> "item label"
  return (id,label)

remarkEditor :: Int -> Parser (Maybe String,[String])  
remarkEditor n = option (Nothing,[]) $ do
  count n (char ',')
  remark <- (Just <$> stringCell) <|> (char ',' >> return Nothing)
  editor <- option [] stringCell
  return (remark,stringSequence editor)

item :: Int -> Parser (Tree Item)
item level = do
  count ((level-1)*2) (char ',')
  (id,label) <- idLabel
  (remark,editor) <- remarkEditor (4 - (level-1)*2)
  nextLine
  optional $ try emptyRow
  xs <- if level==3 then return [] else many . try . item $ level + 1
  return $ Node (Item (toEnum level) id label [] Nothing editor remark) xs

emptyRow :: Parser ()
emptyRow = manyTill (char ',') newline >> return ()

------------------------------------------------------------------------------
-- CSV output
------------------------------------------------------------------------------

csvCatalogue :: Catalogue -> String
csvCatalogue c@(Node x ns) = 
  "FER3 Knowledge Catalogue,,,,,,,\n" ++
  printf "Catalogue:,,%s,,,,,\nCatalogue ID:,,%s,,,,,\nVersion:,,%s,,,,,\nDate:,\n"
    (csvQuote $ itemLabel x) (itemId x) (fromMaybe "NA" $ itemVersion x) ++
  printf "Editor(s):,,%s,,,,,\nComment:,,%s\n" 
    (csvQuote . showSequence $ itemEditors x) 
    (csvQuote . fromMaybe "" $ itemRemark x) ++
  ",,,,,,,\n" ++
  "KA ID,KA Name,KU ID,KU Name,KT ID,KT Name,Comment,Editor(s)\n" ++
  concatMap (csvItem 0) ns

csvItem :: Int -> Catalogue -> String
csvItem n c@(Node x ns) = 
  replicate (n*2) ',' ++
  csvQuote (itemId x) ++ "," ++ 
  csvQuote (itemLabel x) ++ "," ++
  replicate (4 - n*2) ',' ++
  csvQuote (fromMaybe "" $ itemRemark x) ++ "," ++
  csvQuote (showSequence $ itemEditors x) ++ "\n" ++
  concatMap (csvItem (n+1)) ns

csvQuote :: String -> String
csvQuote s | needsQuotes = "\"" ++ escape s ++ "\""
           | otherwise = s
  where needsQuotes = any (==',') s
        escape [] = []
        escape ('"':xs) = "\"\"" ++ escape xs
        escape (x:xs)   = x : escape xs

------------------------------------------------------------------------------
-- Fix catalogue
------------------------------------------------------------------------------

fixIds :: Catalogue -> Catalogue
fixIds (Node x ns) = Node x $
  zipWith f (map (itemId . rootLabel) ns) ns
  where f id (Node x xs) = Node (x { itemId = id}) 
          (zipWith f (ids (itemId x)) xs)
        ids p = [ printf "%s%02d" p i | i <- [(1::Int)..]]

prefixRootId :: Catalogue -> Catalogue
prefixRootId (Node x xs) = Node x (map (fmap f) xs)
  where f y = y { itemId = itemId x ++ "-" ++ itemId y }

fixEditors :: Catalogue -> Catalogue
fixEditors c@(Node x ns) = fix [] (Node x' ns)
  where fix e (Node x ns) = 
          let e' = if null (itemEditors x) then e else itemEditors x
          in  Node (x { itemEditors = e' } ) $ map (fix e') ns
        x' | itemType x == CAT = 
               x { itemEditors = map (++ " (CE)") $ itemEditors x }
           | otherwise = x

fixPaths :: Catalogue -> Catalogue
fixPaths c = fix [] c
  where fix z (Node x ns) = 
          let z' = z ++ [printf "[%s] %s" (itemId x) (itemLabel x)]
          in Node (x { itemPath = z' }) $ map (fix z') ns

fixVersion :: Catalogue -> Catalogue
fixVersion c@(Node x ns) = fix (itemVersion x) c
  where fix v (Node x ns) = Node (x { itemVersion = v }) $ map (fix v) ns

fixLabels :: Catalogue -> Catalogue
fixLabels = fmap (\x -> x {itemLabel = fixLabel (itemLabel x)})

-- Remove superflous spaces, capitalize sublabels, remove trailing puncuation
fixLabel :: String -> String
fixLabel [] = []
fixLabel s  = 
  rtp . intercalate "; " . map upper . splitOn "; " . unwords $ words s
  where upper (c:cs) = toUpper c : cs
        rtp = reverse . dropWhile isPunct . reverse
        isPunct c = c=='.' || c==';'

fixCatalogue :: Catalogue -> Catalogue
fixCatalogue = fixPaths . fixEditors . fixLabels . prefixRootId . fixIds

------------------------------------------------------------------------------
-- Semantic representations
------------------------------------------------------------------------------

type SemCatalogue a = Tree (Item,a)

class SemRep a where
  compare :: a -> a -> Double
  compose :: a -> a -> a

semCatalogue :: SemRep a => (String -> a) -> Catalogue -> SemCatalogue a
semCatalogue f = fmap (\x -> (x,f $ itemLabel x))

------------------------------------------------------------------------------
-- Simple BOW representation
------------------------------------------------------------------------------

normalize :: String -> String
normalize = Stemmer.stem Stemmer.English . lower
  where lower s | any isLower s = map toLower s
                | otherwise     = s

tokenize :: String -> [String]
tokenize = 
  filter (not . null) . concatMap slashSplit . splitOneOf " .,:;’'`\"()[]“”"
  where notAcronym = any isLower
        slashSplit s = if notAcronym s then splitOn "/" s else [s]

newtype Bow = Bow (Set String) deriving (Eq,Show)

instance SemRep Bow where
  compare (Bow xs) (Bow ys) = dice xs ys
  compose (Bow xs) (Bow ys) = Bow $ S.union xs ys

dice :: (Ord a, Eq a) => Set a -> Set a -> Double
dice xs ys = 
  (realToFrac $ 2 * S.size zs) / (realToFrac $ S.size xs + S.size ys)
  where zs = S.intersection xs ys 

bow :: [String] -> String -> Bow
bow sw = Bow . S.map normalize . S.filter f . S.fromList . tokenize
  where f s = length s > 2 && s `notElem` sw

semCatalogueBow :: [String] -> Catalogue -> SemCatalogue Bow
semCatalogueBow sw = semCatalogue (bow sw)

------------------------------------------------------------------------------
-- TfIdf BOW representation
------------------------------------------------------------------------------

type Vector a = Map a Double
newtype TfIdfBow = TfIdfBow (Vector String) deriving (Eq,Show)

type IdfMap = Map String Double

idfMap :: Catalogue -> IdfMap
idfMap c = M.fromList . map (\t -> (t, idf n ds t)) $ catTerms c
  where n  = length ls
        ls = catLabels c
        ds = map (map normalize . tokenize) ls

idf :: Int -> [[String]] -> String -> Double
idf n ds t | df==0     = 0
           | otherwise = log $ (realToFrac n) / df
  where df = realToFrac . length $ filter (t `elem`) ds

catTerms :: Catalogue -> [String]
catTerms = concatMap (map normalize . tokenize) . catLabels
 
tfIdfBow :: [String] -> IdfMap -> String -> TfIdfBow
tfIdfBow sw m s = 
  TfIdfBow . M.fromList $ map (\t -> (t, tf t * idf t)) ts
  where ts    = map normalize . filter f $ tokenize s
        tf t  = realToFrac . length $ filter (==t) ts
        idf t = M.findWithDefault 0 t m
        f t   = length t > 2 && t `notElem` sw

instance SemRep TfIdfBow where
  compare (TfIdfBow x) (TfIdfBow y) = cosine x y
  compose (TfIdfBow x) (TfIdfBow y) = TfIdfBow $ M.unionWith (+) x y

cosine :: Ord a => Vector a -> Vector a -> Double
cosine x y = dot x y / (sqrt (dot x x) * sqrt (dot y y))
  where dot x y = sum . map snd . M.toList $ M.intersectionWith (*) x y

semCatalogueTfIdf :: [String] -> Catalogue -> SemCatalogue TfIdfBow
semCatalogueTfIdf sw c = semCatalogue (tfIdfBow sw (idfMap c)) c

------------------------------------------------------------------------------
-- Similarity computation
------------------------------------------------------------------------------

type CatalogueSim a = SemCatalogue a -> SemCatalogue a -> Double

sim1 :: SemRep a => CatalogueSim a
sim1 (Node (_,r1) _) (Node (_,r2) _) = compare r1 r2

sim2 :: SemRep a => CatalogueSim a
sim2 c1 c2 = compare r1 r2
  where r1 = foldl1 compose . map snd $ flatten c1
        r2 = foldl1 compose . map snd $ flatten c2

sim3 :: (SemRep a, Eq a) => CatalogueSim a
sim3 c1@(Node (x1,r1) _) c2@(Node (x2,r2) _)
  | ((itemType x1 == KT && itemType x2 /= KT) ||
    (itemType x1 /= KT && itemType x2 == KT)) && (not $ sameArea c1' c2')
    = 1 - sqrt (1 - compare r1 r2)
  | itemType x1 `elem` [KA,KU] && itemType x2 `elem` [KA,KU] && (not $ sameArea c1' c2')
    = compare r1' r2'
  | otherwise = 0
  where 
    r1' = foldl1 compose . map snd $ flatten c1
    r2' = foldl1 compose . map snd $ flatten c2
    c1' = fmap fst c1
    c2' = fmap fst c2
 
samePath :: Catalogue -> Catalogue -> Bool   
samePath c1 c2 = c1 `isSubtree` c2 || c2 `isSubtree` c1

-- tmp, because there are no backpointers!
sameArea :: Catalogue -> Catalogue -> Bool
sameArea c1 c2 = a1 `isPrefixOf` a2 || a2 `isPrefixOf` a1
  where a1 = areaId $ rootLabel c1
        a2 = areaId $ rootLabel c2
--sameArea c c1 c2 = any (\a -> c1 `isSubtree` a && c2 `isSubtree` a) areas
--  where areas = subForest c

areaId :: Item -> String
areaId = takeWhile (not . isDigit) . itemId

minSim = 0.33

catCompareOn :: SemRep a =>
  CatalogueSim a -> SemCatalogue a -> [SemCatalogue a] -> [(Double,Item)]
catCompareOn f x = 
  sortBy (flip $ comparing fst) . filter ((>=minSim) . fst) . 
  map (\y -> (f x y, fst $ rootLabel y))

-- catCompare f x c: compares x against all subcatalogues of c,
-- excluding x, all supercatalogues of x, and all subcatalogues of x
catCompare :: (SemRep a, Eq a) =>
  CatalogueSim a -> SemCatalogue a -> SemCatalogue a -> [(Double,Item)]
catCompare f x = catCompareOn f x . subtrees
  --where sc = filter (\y -> not (y `isSubtree` x || x `isSubtree` y)) $ subtrees c

subtrees :: Tree a -> [Tree a]
subtrees n@(Node _ ns) = n : concatMap subtrees ns
--subtrees n@(Node _ ns) = concatMap subtrees ns

-- subtree test (not invariant to subforest order!)
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree x@(Node _ xs) y@(Node _ ys) = x==y || any (isSubtree x) ys

subtreesTree :: Tree a -> Tree (Tree a)
subtreesTree n@(Node _ ns) = Node n (map subtreesTree ns)

type SimRank = [(Double,Item)]

type SimCatalogue = Tree (Item, SimRank)

simCatalogue :: (SemRep a, Eq a) => 
  CatalogueSim a -> SemCatalogue a -> SimCatalogue
simCatalogue s c = 
  fmap (\n -> (fst $ rootLabel n, catCompare s n c)) $ subtreesTree c

------------------------------------------------------------------------------
-- HTML output
------------------------------------------------------------------------------

htmlPage :: String -> String
htmlPage s = 
  "<html>\n<head>\n" ++
  "<meta charset=\"UTF-8\">" ++
  "<title>FER3 Knowledge Catalogue</title>\n" ++
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"fer3.css\"></head>\n" ++
  "<body>\n" ++ s ++ "\n<br></body>\n</html>"

htmlCatalogue :: Catalogue -> String
htmlCatalogue c@(Node x ns) = htmlPage $ 
  "<h1>FER3 Knowledge Catalogue</h1>" ++
  printf "<p>Version: %s (<a href=\"FER3-editors.html\">Editors...</a>)<br>"
    (fromMaybe "NA" $ itemVersion x) ++
  printf "%d areas, %d units, %d topics</p>" ka ku kt ++
  "<table><tr><th>Knowledge Area</th><th>Knowledge Unit</th><th>Knowledge Topic</th></tr>" ++
  concatMap (htmlNode 0) ns ++ "</table>"
  where (ka,ku,kt) = catStats c

htmlNode :: Int -> Catalogue -> String
htmlNode i (Node x xs) = 
  "<tr>" ++
  concat (replicate i "<td></td>") ++
  printf "<td colspan=\"%d\"><a id=\"%s\"></a>%s</td></tr>\n"
    (3-i) (itemId x) (htmlItem x) ++
  concatMap (htmlNode (i+1)) xs

htmlItem :: Item -> String
htmlItem x = 
  printf "<span class=\"%s-font\" title=\"%s\">[%s] %s</span>" 
    (show $ itemType x) (infoText::String) (itemId x) (itemLabel x)
  where infoText = printf "%s %s\n\n%s\n\nVersion: %s\nEditor(s): %s\nRemark: %s" 
                     (itemTypeName x) (itemId x) 
                     (intercalate "\n -> " $ itemPath x)
                     (optText itemVersion)
                     (showSequence $ itemEditors x)
                     (optText itemRemark)
        optText f = fromMaybe "NA" $ f x

htmlItem2 :: Item -> String
htmlItem2 x = 
  printf ("<span class=\"%s-font\" title=\"%s\">[%s]</span></td><td>" ++ 
         "<span class=\"%s-font\" title=\"%s\">%s</span>")
    (show $ itemType x) (infoText::String) (itemId x) 
    (show $ itemType x) (infoText::String) (itemLabel x) 
  where infoText = printf "%s %s\nVersion: %s\nEditor(s): %s\nRemark: %s" 
                     (itemTypeName x) (itemId x) 
                     (optText itemVersion)
                     (showSequence $ itemEditors x)
                     (optText itemRemark)
        optText f = fromMaybe "NA" $ f x

showSequence :: [String] -> String
showSequence = intercalate ", "

itemTypeName :: Item -> String 
itemTypeName x = case itemType x of
  KA -> "Knowledge Area"
  KU -> "Knowledge Unit"
  KT -> "Knowledge Topic"
  CAT -> "Catalogue"

---

type Thresholds = (Double,Double)

htmlSimCatalogue :: Thresholds -> SimCatalogue -> String
htmlSimCatalogue ts c@(Node (x,_) ns) = htmlPage $ 
  "<h1>FER3 Knowledge Catalogue</h1>" ++
  printf "<p>Version: %s (<a href=\"FER3-editors.html\">Editors...</a>)<br>"
    (fromMaybe "NA" $ itemVersion x) ++
  printf "%d areas, %d topics, %d units</p>" ka ku kt ++
  "<table><tr><th>Knowledge Area</th><th>Knowledge Unit</th><th>Knowledge Topic</th><th>SimIndex</th></tr>" ++
  concatMap (htmlSimNode 0 ts) ns ++ 
  "</table>" ++
  htmlSimRanks ts c
  where (ka,ku,kt) = catStats $ fmap fst c

htmlSimNode :: Int -> Thresholds -> SimCatalogue -> String
htmlSimNode i ts n@(Node (x,r) xs) = 
  "<tr>" ++
  concat (replicate i "<td></td>") ++
  printf "<td colspan=\"%d\"><a id=\"%s\"></a><a href=#sim-%s><span class=\"%s-font\">%s</span></a></td>" 
    (3-i) (itemId x) (itemId x) (simIndexColor ts si) (htmlItem x) ++ 
  showSim ++ "</tr>\n" ++ 
  concatMap (htmlSimNode (i+1) ts) xs
  where simIndex ((i,_):_) = i
        simIndex []        = 0
        si = simIndex r
        showSim = printf "<td><span class=\"%s-font\">%.0f%%</span></td>" (simIndexColor ts si) (si * 100)

simIndexColor :: Thresholds -> Double -> String
simIndexColor (t1,t2) i | i >= t2   = "sim-high"
                        | i >= t1   = "sim-med"
                        | otherwise = "sim-low"

htmlSimRanks :: Thresholds -> SimCatalogue -> String
htmlSimRanks ts = unlines . map htmlRanks . filter (not . null . snd) . flatten
  where htmlRanks (x, r) = 
          printf "<a id=\"sim-%s\" href=#%s><h2>[%s] %s</h2></a>"
            (itemId x) (itemId x) (itemId x) (itemLabel x) ++
          printf "<p>Editor(s): <i>%s</i><br>Remark: <i>%s</i></p>"
            (htmlEditorsInline x)
            (fromMaybe "NA" $ itemRemark x) ++ 
          "<table><th></th><th>Knowledge Area/Unit/Topic</th><th>Editor(s)</th>" ++
          concatMap htmlSimItem r ++ "</table>"
        htmlSimItem (i,x) = 
          printf "<tr><td><span class=\"%s-font\">%.0f%%</span></td><td><a href=#%s><span class=\"item-font\">%s</span></a></td><td><i>%s</i></td></tr>"
          (simIndexColor ts i) (i * 100) (itemId x) (htmlItem x)
          (htmlEditorsInline x)

htmlEditorsInline :: Item -> String
htmlEditorsInline = intercalate ", " . map f . itemEditors
  where f s = printf "<a href=%s>%s</a>" (editorLink s) s

htmlEditors :: Catalogue -> String
htmlEditors c@(Node x _) = htmlPage $ 
  "<h1>FER3 Knowledge Catalogue Editors</h1>" ++
  printf "<p>Version: %s (<a href=\"FER3.html\">Catalogue...</a>)<br>"
    (fromMaybe "NA" $ itemVersion x) ++
  printf "%d editors</p>" (length es) ++
  concatMap (htmlEditor c) es
  where (ka,ku,kt) = catStats c
        es = catEditors c

htmlEditor :: Catalogue -> String -> String
htmlEditor c e = 
  printf "<a id=\"%s\"></a><h2>%s</h2>" (filter isAlpha e) e ++
  "<table><th>Knowledge Area/Unit/Topic</th>" ++
  concatMap (\x -> printf "<tr><td><a href=%s><spann class=\"item-font\">%s</s></a></td></tr>" 
    (itemLink x) (htmlItem x))
    (map rootLabel $ catEditedBy e c) ++ 
  "</table>"

itemLink :: Item -> String
itemLink x = "FER3.html#" ++ itemId x

editorLink :: String -> String
editorLink = ("FER3-editors.html#" ++) . filter isAlpha

------------------------------------------------------------------------------
-- CSV SimCat ranks output
------------------------------------------------------------------------------

csvSimRanks :: SimCatalogue -> String
csvSimRanks = unlines . map csvRanks . filter (not . null . snd) . flatten
  where csvRanks (x,r) = 
          printf "[%s],%s,,,,%s\n" 
            (itemId x) (csvQuote $ itemLabel x)
            (csvQuote . intercalate ", " $ itemEditors x) ++ 
            concatMap csvSimItem r
        csvSimItem (i,x) = 
          printf ",%.0f%%,[%s],%s,%s\n"
          (i * 100) (itemId x) (csvQuote $ itemLabel x)
          (csvQuote . intercalate ", " $ itemEditors x)

csvSimRanks2 :: (Item -> Bool) -> SimCatalogue -> String
csvSimRanks2 p = 
  unlines . map csvRanks . filter (p . fst) . filter (not . null . snd) . flatten
  where csvRanks (x,r) = 
          printf "[%s],%s,,,,%s\n" 
            (itemId x) (csvQuote $ itemLabel x)
            (csvQuote . intercalate ", " $ itemEditors x) ++ 
            concatMap csvSimItem r
        csvSimItem (i,x) = 
          printf ",%.0f%%,[%s],%s,%s\n"
          (i * 100) (itemId x) (csvQuote $ itemLabel x)
          (csvQuote . intercalate ", " $ itemEditors x)

------------------------------------------------------------------------------

mergeCatalogues :: [Catalogue] -> Catalogue
mergeCatalogues cs = 
  Node (Item CAT "FER3" "FER3" [] (Just catVersion) [] Nothing) $ 
  concatMap subForest cs

inDir  = "/home/jan/fer3/granule/v1/in"
outDir = "/home/jan/fer3/granule/v1/out"

catFiles = [
    "Automatika_granule_V4_AUT.csv"
  , "FER3_CE_RASIP_20140924.csv"
  , "FER3-Knowledge-Catalogue-CS-v6.csv"
  , "Automatika_granule_V4_CSY.csv"
  , "FER3_Knowledge_Catalogue_ZEA.csv"
  , "Granule_elektronika_mikroelektronika_catalogue_engl_final_3.csv"
  , "ECE_KnowledgeUnits_2014_7_22.csv"
  , "FER3_Knowledge_Catalogue_FEE_M_v2-4.csv"
  , "ZESA_granule_EN_16_09_2014.csv"
  , "FER3_EnergyPowerSystems_Knowledge_Units_v1.01.csv"
  , "IP_KnowledgeUnits_2014_7_22.csv"
  , "FER3_-_Mathematics.csv"
  , "FER3_Knowledge_Catalogue_Physics.csv"
  , "Automatika_granule_V4_RES.csv"
  , "Automatika_granule_V4_ROB.csv"
  , "FER3-Knowledge-Catalogue-SE-1.0.csv"
  , "Područja i granule znanja (TKI) - mreže  - 07092014 - za Povjerenstvo.csv"
  , "FER3_WT_Knowledge_Catalogue-1.csv"]

swFile  = "data/stopwords.txt"

thresholds1 = (0.50,0.75)
thresholds2 = (0.33,0.66)

catVersion = "1.0"

main = do
  sw <- lines <$> readFile swFile
  cs <- forM catFiles $ \f -> do
          c <- fixVersion . fixLabels . fixIds . readCatalogue <$> readFile (inDir </> f)
          let (ka,ku,kt) = catStats c
              areaId     = itemId $ rootLabel c
              ver        = fromMaybe "" . itemVersion $ rootLabel c
          putStrLn $ printf "Catalogue %s: %d areas, %d units, %d topics"
                            areaId ka ku kt
          writeFile (outDir</>"csv"</> printf "FER3-%s-v%s.csv" areaId ver) $ 
            csvCatalogue c
          return (fixPaths . fixEditors $ prefixRootId c)
  let c   = mergeCatalogues cs
      (ka,ku,kt) = catStats c
      --sc1 = semCatalogueBow sw c
      semCat = semCatalogueTfIdf sw c
      simCat = simCatalogue sim3 semCat
  putStrLn $ printf "TOTAL: %d areas, %d units, %d topics" ka ku kt
  writeFile (outDir</>"csv"</> printf "FER3-v%s.csv" catVersion) $ 
    csvCatalogue c
  writeFile (outDir</>"html"</>"FER3.html") $ 
   htmlCatalogue c
  writeFile (outDir</>"html"</>"FER3-editors.html") $ 
    htmlEditors c
--  writeFile (outDir</>"sim"</>"FER3-sim.txt") $ show simCat

outputSimCatHtml = do
  simCat <- read <$> readFile (outDir</>"sim"</>"FER3-sim.txt") :: IO SimCatalogue
  writeFile (outDir</>"html"</>"FER3-sim.html") $ 
    htmlSimCatalogue thresholds2 simCat

cats :: SimCatalogue -> [String]
cats = map catName . map (fst . rootLabel) . subForest  

catName :: Item -> String
catName = takeWhile (/='-') . itemId

outputSimRanksCsv = do
  simCat <- read <$> readFile (outDir</>"sim"</>"FER3-sim.txt") :: IO SimCatalogue
  forM_ (cats simCat) $ \c ->
    writeFile (outDir</>"csv"</>(printf "FER3-%s.sim.csv" c)) $ 
      csvSimRanks2 ((==c) . catName) simCat

