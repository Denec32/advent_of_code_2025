solve :: [String] -> Int -> Int -> Int

solve [] position zeroPoints = zeroPoints

solve (current : others) position zeroPoints =
    let turn :: Int = read (tail current) `mod` 100
        fullTurns = abs (read (tail current) `div` 100)
        newPosition = turnDial (head current) turn position
        addition = if (newPosition >= 100 || newPosition <= 0) && position /= 0 then 1 else 0
    in solve others ((newPosition + 100) `mod` 100) (zeroPoints + fullTurns + addition)

turnDial :: Char -> Int -> Int -> Int

turnDial 'R' turn base = base + turn

turnDial 'L' turn base = base - turn

main = do
    let file = "input.txt"
    content <- readFile file

    print (solve (lines content) 50 0)