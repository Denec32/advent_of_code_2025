solve :: [String] -> Int -> Int -> Int

solve [] position zeroPoints = zeroPoints

solve (current : others) position zeroPoints =
    let newPosition = turnDial (head current) (read (tail current)) position
    in solve others newPosition (countTurn newPosition + zeroPoints)

turnDial :: Char -> Int -> Int -> Int

turnDial 'R' turn base = base + turn

turnDial 'L' turn base = base - turn

countTurn :: Int -> Int

countTurn position | position `mod` 100 == 0 = 1
                   | otherwise = 0

main = do
    let file = "input.txt"
    content <- readFile file

    print (solve (lines content) 50 0)