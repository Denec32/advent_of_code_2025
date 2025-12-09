import Data.List
import Data.Maybe (fromMaybe)
import Text.Parsec (parse)
import Data.Ord (comparing)

import Debug.Trace

parseRanges :: [String] -> [(Int, Int)]

parseRanges = map parseRange 

parseRange :: [Char] -> (Int, Int)

parseRange currentRange =
    let separatorIdx = fromMaybe 0 (elemIndex '-' currentRange)
        firstValue :: Int = read (take separatorIdx currentRange)
        secondValue :: Int = read (drop (separatorIdx + 1) currentRange)
    in (firstValue, secondValue)

solve :: [(Int, Int)] -> Int

solve ingredientRanges = 
    let sortedRanges = sortBy (comparing fst) ingredientRanges
        mergedRanges = mergeRanges sortedRanges
    in foldr ((+) . countItems) 0 mergedRanges

mergeRanges :: [(Int, Int)] -> [(Int, Int)]

mergeRanges [] = []

mergeRanges [x] = [x]

mergeRanges (range1:range2:otherRanges)
    | canMerge range1 range2 = mergeRanges (merge range1 range2 : otherRanges)
    | otherwise = range1 : mergeRanges (range2:otherRanges)

canMerge :: (Int, Int) -> (Int, Int) -> Bool

canMerge range1 range2 = snd range1 >= fst range2

merge :: (Int, Int) -> (Int, Int) -> (Int, Int)

merge range1 range2 = (fst range1, max (snd range1) (snd range2))

countItems :: (Int, Int) -> Int 
countItems (first, last) = last - first + 1

main = do
    let file = "input.txt"
    content <- readFile file

    let separatorIdx = fromMaybe 0 (elemIndex "" (lines content))

    let idRanges = take separatorIdx (lines content)

    print (solve (parseRanges idRanges))