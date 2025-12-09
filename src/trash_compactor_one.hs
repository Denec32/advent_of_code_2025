import Data.List

solve :: [[String]] -> [String] -> Int

solve operands operators = foldr ((+) . solveProblem) 0 (zip operands operators)

solveProblem :: ([String], String) -> Int

solveProblem (rawNumbers, rawOperator) =
    let parsedNumbers :: [Int] = map read rawNumbers
    in if rawOperator == "+"
        then sum parsedNumbers
        else product parsedNumbers

main = do
    let file = "input.txt"
    content <- readFile file

    let matrix = foldr ((:) . words) [] (lines content)

    let operands = transpose (init matrix)
    let operators = last matrix

    print (solve operands operators)