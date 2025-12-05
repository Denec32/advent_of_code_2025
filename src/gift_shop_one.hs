replace :: Char -> Char -> String -> String

replace old new = map (\x -> if x == old then new else x)

solve :: [String] -> Int

solve = foldr ((+) . findRepeatedInRange) 0

findRepeatedInRange :: String -> Int

findRepeatedInRange rawRange =
    let parsedRangeList = words (replace '-' ' ' rawRange)
        start = head parsedRangeList
        end = last parsedRangeList
        parsedRange = [read start :: Int .. read end :: Int]
    in foldr ((+) . isRepeatedTwice) 0 parsedRange

isRepeatedTwice :: Int -> Int
isRepeatedTwice x =
    let len = length (show x)
    in if even len && take (len `div` 2) (show x) == drop (len `div` 2) (show x) then x else 0

main = do
    let file = "input.txt"
    content <- readFile file

    let replaced = replace ',' ' ' content

    print (solve (words replaced))