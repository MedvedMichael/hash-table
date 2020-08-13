
import HashTable
import Prelude

import Data.Array((!))
import Data.List.Split
import Data.List(findIndex,)
import Data.Char(toUpper)

listToStr :: Int -> String
listToStr 0 = ""
listToStr num = show num ++ "; " ++ show num ++ "\n" ++ listToStr (num-1)

main :: IO()
main = do
    -- writeFile "output.txt" $ listToStr 100 
    text <- readFile "output.txt"
    let lines = splitOn "\n" text 
        table = foldl (\b a -> addToTable a b) getEmptyTable lines
        Just currentValue = get table "b"
        
    -- putStrLn $ show $ pairs table
    -- let new = map (\a -> a:" ") text
    -- putStrLn $ show new
    loop table

loop :: HashTable -> IO()
loop table = do 
    putStrLn "Insert the word: " 
    line <- getLine
    if line == ":q" then putStrLn "Bye" >> return () else do
        let value = get table $ map toUpper line
        case value of
            Nothing -> putStrLn "No such word" >> loop table
            Just value' -> putStrLn (show value') >> loop table



addToTable :: String -> HashTable -> HashTable
addToTable line table = 
    let (HashTable pairs hash) = table in do
        let index = findIndex (\a -> a == ';' ) line
        if not $ index == Nothing then do
            let Just index' = index
                (key, value) = splitAt index' line 
                a:b:value' = value
            put table (KeyValuePair key value' Nothing)
        else table


findFirstIndex :: String -> Char -> Int -> Int
findFirstIndex (a:as) c num = if a == c then num else findFirstIndex as c num+1
