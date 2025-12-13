import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

solve :: [String] -> Set Int -> Int

solve [] queue = 0

solve (currentLevel:otherLevels) queue =
    let (splits, newQueue) = collideSplitters currentLevel queue
    in splits + solve otherLevels newQueue

collideSplitters :: String -> Set Int -> (Int, Set Int)

collideSplitters level queue = foldr (sumTuple . visit level) (0, Set.empty) (Set.toList queue)

visit :: String -> Int -> (Int, Set Int)

visit level pos = if level !! pos == '^' then (1, Set.fromList [pos - 1, pos + 1]) else (0, Set.singleton pos)

sumTuple :: (Int, Set Int) ->  (Int, Set Int) ->  (Int, Set Int)

sumTuple (firstSum, firstQueue) (secondSum, secondQueue) = (firstSum + secondSum, firstQueue `Set.union` secondQueue)

main = do
    let file = "input.txt"
    content <- readFile file
    let levels = lines content
    let start = fromMaybe 0 (elemIndex 'S' (head levels));

    print (solve (tail levels) (Set.singleton start))