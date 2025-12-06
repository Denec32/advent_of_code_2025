solve :: [String] -> Int

solve = foldr ((+) . parseAndFindMaxJoltage) 0

parseAndFindMaxJoltage :: String -> Int

parseAndFindMaxJoltage nums = read (maxJoltage (map (\x -> fromEnum x - fromEnum '0') nums) 0 12)

maxJoltage ::[Int] -> Int -> Int -> String

maxJoltage nums startIdx 0 = ""

maxJoltage nums startIdx left = 
    let startNums = drop startIdx nums
        endNums = take (length startNums - left + 1) startNums
        (idx, num) = findHighestNumber endNums 0
    in show num ++ maxJoltage nums (startIdx + idx + 1) (left - 1)

findHighestNumber :: [Int] -> Int -> (Int, Int)

findHighestNumber [last] currentIdx = (currentIdx, last)

findHighestNumber (current : other) currentIdx =
    let (idx, num) = findHighestNumber other (currentIdx + 1)
    in if current >= num 
        then (currentIdx, current) 
        else (idx, num)


main = do
    let file = "input.txt"
    content <- readFile file

    print (solve (lines content))