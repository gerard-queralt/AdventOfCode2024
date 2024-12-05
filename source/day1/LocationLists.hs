import HaskellFileReader
import Data.List

computeDistanceRecursive :: Int -> [Int] -> [Int] -> Int
computeDistanceRecursive acc [] [] = acc
computeDistanceRecursive acc _ [] = acc
computeDistanceRecursive acc [] _ = acc
computeDistanceRecursive acc (l:ls) (r:rs) = computeDistanceRecursive (acc + abs (l - r)) ls rs

countOccurrences :: Int -> [Int] -> Int
countOccurrences _ [] = 0
countOccurrences x (y:ys)
  | x == y    = 1 + countOccurrences x ys
  | otherwise = countOccurrences x ys

splitLists :: [String] -> ([Int], [Int])
splitLists listOfPairs = unzip $ map ((\[l,r] -> (read l, read r)) . words) listOfPairs

sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (lList, rList) = (sort lList, sort rList)

computeDistance :: [String] -> Int
computeDistance listOfPairs = sumOfDistances
  where
    (lList, rList) = sortLists $ splitLists listOfPairs
    sumOfDistances = computeDistanceRecursive 0 lList rList

computeSimilarity :: [String] -> Int
computeSimilarity listOfPairs = sumOfSimilarities
  where
    (lList, rList) = splitLists listOfPairs
    sumOfSimilarities = sum $ map (\x -> x * countOccurrences x rList) lList

main :: IO ()
main = do
  input <- readInputOfDay 1
  print $ "Part 1: " ++ show (computeDistance $ lines input)
  print $ "Part 2: " ++ show (computeSimilarity $ lines input)
