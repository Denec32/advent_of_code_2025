solve :: [String] -> Int

solve = foldr ((+) . parseAndFindMaxJoltage) 0

parseAndFindMaxJoltage :: String -> Int

parseAndFindMaxJoltage nums = maxJoltage (map (\x -> fromEnum x - fromEnum '0') nums) 0 0

maxJoltage :: [Int] -> Int -> Int -> Int

maxJoltage [] major minor = major * 10 + minor

maxJoltage [lastDigit] major minor = maxJoltage [] major (max minor lastDigit)

maxJoltage (currentDigit : otherDigits) major minor = 
    if currentDigit > major 
        then maxJoltage otherDigits currentDigit 0
        else maxJoltage otherDigits major (max currentDigit minor)

main = do
    let file = "input.txt"
    content <- readFile file

    print (solve (lines content))