import Data.List
import Data.Maybe (fromMaybe)

parseRanges :: [String] -> [(Int, Int)]

parseRanges = map parseRange 

parseRange :: [Char] -> (Int, Int)

parseRange currentRange =
    let separatorIdx = fromMaybe 0 (elemIndex '-' currentRange)
        firstValue :: Int = read (take separatorIdx currentRange)
        secondValue :: Int = read (drop (separatorIdx + 1) currentRange)
    in (firstValue, secondValue)


solve :: [(Int, Int)] -> [Int] -> Int

solve ingredientRanges = foldr ((+) . (\x -> if isInRange ingredientRanges x then 1 else 0)) 0

isInRange :: [(Int, Int)] -> Int -> Bool

isInRange ingredientRanges ingredient = 
    foldr ((||) . (\ingredientRange -> fst ingredientRange <= ingredient && snd ingredientRange >= ingredient)) False ingredientRanges

main = do
    let file = "input.txt"
    content <- readFile file

    let separatorIdx = fromMaybe 0 (elemIndex "" (lines content))

    let idRanges = take separatorIdx (lines content)

    let ingredients :: [Int] = map read (drop (separatorIdx + 1) (lines content))

    print (solve (parseRanges idRanges) ingredients)