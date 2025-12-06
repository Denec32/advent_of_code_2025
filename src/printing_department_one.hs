solve :: [String] -> Int -> Int
solve matrix idx =
    let n = length matrix
        m = length (head matrix)
        r = idx `div` n
        c = idx `mod` m
    in if idx == m * n
        then 0
        else solve matrix (idx + 1) + if (matrix !! r) !! c == '@' && canAccess matrix r c then 1 else 0 

canAccess :: [String] -> Int -> Int -> Bool

canAccess matrix r c =
    let directions :: [[Int]] = [[0, 1], [1, 0], [-1, 0], [0, -1], [-1, 1], [1, 1], [1, -1], [-1, -1]]
    in foldr (\ [dr, dc] acc -> if isEmptyAt matrix (r + dr) (c + dc) then acc else acc + 1) 0 directions < 4

isEmptyAt :: [String] -> Int -> Int -> Bool

isEmptyAt matrix r c =
    let n = length matrix
        m = length (head matrix)
    in r < 0 || c < 0 || r >= n || c >= m || (matrix !! r) !! c == '.'

main = do
    let file = "input.txt"
    content <- readFile file

    print (solve (lines content) 0)