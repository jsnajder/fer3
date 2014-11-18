module Catalogue where

import Control.Applicative ((<$>))
import Data.List
import qualified Data.Map as M
import Data.Maybe

data ItemType = CAT | KA | KU | KT deriving (Eq,Show,Enum,Read,Ord)

type ItemEditor = String
type ItemId = String

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
  | Alias

type Catalogue = LabeledGraph Item Link

getItem :: Catalogue -> (Item -> Bool) -> Maybe Item
getItem c p = getVertex p c

getItemById :: Catalogue -> ItemId -> Maybe Item
getItemById c id = getItem c ((==id) . itemId)

