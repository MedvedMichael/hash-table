-- module HashTable (HashTable, put, get, getEmptyTable) where
module HashTable where

import Data.Array
import Data.Hashable(hash)
import Prelude

data HashTable = HashTable { 
    pairs :: Table,
    hashFunction :: String -> Int
}

getEmptyTable :: HashTable
getEmptyTable = let len = 5 in 
    HashTable (listArray (0, len-1) $ replicate len Nothing) hash

type Table = Array Int (Maybe KeyValuePair)

data KeyValuePair = KeyValuePair Key Value Next

type Next = Maybe KeyValuePair

type Key = String

type Value = String

addToLast :: Maybe KeyValuePair -> KeyValuePair -> KeyValuePair
addToLast current toAdd = case current of  
    Nothing -> toAdd
    Just (KeyValuePair key value next) -> KeyValuePair key value (Just (addToLast next toAdd))

put :: HashTable -> KeyValuePair -> HashTable
put (HashTable pairs hash) pair =
  let index = rem (abs $ hash key) (length pairs)
      (KeyValuePair key value next) = pair in 
          let current =  pairs ! index in HashTable (pairs // [(index, Just $ addToLast current pair)]) hash

findFromFirst :: Maybe KeyValuePair -> Key -> Maybe KeyValuePair
findFromFirst current key = case current of
    Nothing -> Nothing
    Just (KeyValuePair key' value' next') -> if key == key' then current else findFromFirst next' key 


get :: HashTable -> Key -> Maybe Value
get (HashTable pairs hash) key = 
    let index = rem (abs $ hash key) (length pairs)
        pair = findFromFirst (pairs ! index) key in
            case pair of
                Nothing -> Nothing
                Just (KeyValuePair _ value' _) -> Just value'
    
instance Show KeyValuePair where
    show (KeyValuePair key value next) = "'" ++ key ++ " - " ++ value ++ ", next: " ++ show next ++ "'"