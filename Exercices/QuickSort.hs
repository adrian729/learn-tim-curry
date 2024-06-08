module Main where

import Data.List

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (p : xs) = quickSort ls ++ p : quickSort rs
  where
    (ls, rs) = partition (<= p) xs

arr = [10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9]

main :: IO ()
main = putStrLn . intercalate ", " . map show $ quickSort arr