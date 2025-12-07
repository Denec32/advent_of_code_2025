import Data.Sequence (Seq(..), fromList, viewr, (|>))
import Data.Set (Set)
import qualified Data.Set as Set

directions :: [(Int, Int)] = [(0, 1), (1, 0), (-1, 0), (0, -1), (-1, 1), (1, 1), (1, -1), (-1, -1)]

fillQueue :: [[Char]] -> Seq (Int, Int)

fillQueue matrix = 
    Data.Sequence.fromList
    [ (i, j) 
    | (i, row) <- zip [0..] matrix
    , (j, x)   <- zip [0..] row
    , x == '@'
    ]

solve :: [[Char]] ->  Set(Int, Int) -> Seq (Int, Int) -> Int

solve matrix cleared Data.Sequence.Empty = 0

solve matrix cleared ((r, c) :<| queue) =
    let (newQueue, newCleared, val) = tryAccess matrix queue cleared (r, c) 
    in val + solve matrix newCleared newQueue

tryAccess :: [[Char]] -> Seq (Int, Int) -> Set(Int, Int) -> (Int, Int) -> (Seq (Int, Int), Set(Int, Int), Int)

tryAccess matrix queue cleared (r, c) =
    if canAccess matrix cleared (r, c) && not (Set.member (r, c) cleared)
    then (emplaceCellsAround matrix cleared queue (r, c), Set.insert (r, c) cleared, 1)
    else (queue, cleared, 0)

emplaceCellsAround :: [[Char]] -> Set(Int, Int) -> Seq (Int, Int) -> (Int, Int) ->  Seq (Int, Int)

emplaceCellsAround matrix cleared queue (r, c) =
    foldr (\(dr, dc) acc -> 
        if isEmptyAt matrix cleared (r + dr, c + dc) 
            then acc 
            else acc |> (r + dr, c + dc)) 
    queue 
    directions

canAccess :: [[Char]] -> Set(Int, Int) -> (Int, Int) -> Bool

canAccess matrix cleared (r, c) =
    foldr (\ (dr, dc) acc -> 
        if isEmptyAt matrix cleared (r + dr, c + dc) 
            then acc 
            else acc + 1) 
    0 
    directions < 4

isEmptyAt :: [[Char]] -> Set(Int, Int) -> (Int, Int) -> Bool

isEmptyAt matrix cleared (r, c) =
    let n = length matrix
        m = length (head matrix)
    in Set.member (r, c) cleared || r < 0 || c < 0 || r >= n || c >= m || (matrix !! r) !! c == '.'

main = do
    let file = "input.txt"
    content <- readFile file
    let matrix = lines content
    let queue = fillQueue matrix
    print (solve matrix Set.empty queue)