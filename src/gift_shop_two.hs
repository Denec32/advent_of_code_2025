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
    in foldr ((+) . isRepeatedAtLeastTwice) 0 parsedRange

isRepeatedAtLeastTwice :: Int -> Int

isRepeatedAtLeastTwice x =
    let possibleLengths = [1 .. length (show x) `div` 2]
    in if foldr ((||) . isRepeated x) False possibleLengths then x else 0


isRepeated :: Int -> Int -> Bool
isRepeated num len =
    let part = take len (show num)
    in (((length (show num) `mod` len) == 0) && (concat (replicate partCount part) == show num))
    where partCount = length (show num) `div` len

main = do
    let file = "input.txt"
    content <- readFile file

    let replaced = replace ',' ' ' content

    print (solve (words replaced))