import HaskellFileReader
import Data.List

computeDistanceRecursive :: Int -> [Int] -> [Int] -> Int
computeDistanceRecursive acc [] [] = acc
computeDistanceRecursive acc _ [] = acc
computeDistanceRecursive acc [] _ = acc
computeDistanceRecursive acc (l:ls) (r:rs) = computeDistanceRecursive (acc + abs (l - r)) ls rs

splitLists :: [String] -> ([Int], [Int])
splitLists listOfPairs = unzip $ map ((\[l,r] -> (read l, read r)) . words) listOfPairs

sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (lList, rList) = (sort lList, sort rList)

computeDistance :: [String] -> Int
computeDistance listOfPairs = sumOfDistances
  where
    (lList, rList) = sortLists $ splitLists listOfPairs
    sumOfDistances = computeDistanceRecursive 0 lList rList

main :: IO ()
main = do
  input <- readInputOfDay 1
  print $ "Part 1: " ++ show (computeDistance $ lines input)
