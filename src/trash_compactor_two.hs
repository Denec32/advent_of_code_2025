import Data.List
import Debug.Trace
import Data.Char
import Data.Maybe (fromMaybe)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

solve :: [String] -> [Char] -> Int

solve [] [] = 0

solve rawNumbers (operator : operators) =
    let portionSize = fromMaybe (length rawNumbers) (elemIndex "" rawNumbers)
        portion = take portionSize rawNumbers
        operationResult = performOperation portion operator
    in operationResult + solve (drop (portionSize + 1) rawNumbers) operators

performOperation :: [String] -> Char -> Int

performOperation numbers operation =
    let parsedNumbers :: [Int] = map read numbers
    in if operation == '+' then sum  parsedNumbers else product parsedNumbers

main = do
    let file = "input.txt"
    content <- readFile file
    let operands = init content
    let operators = reverse (filter (not . isSpace) (last (lines content)))

    let rawNumbers = (map trim . transpose . map reverse . init . lines) operands
    print (solve rawNumbers operators)