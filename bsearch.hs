-- binary search

bsearch :: [Int] -> Int -> Int
bsearch a k = search a k 0 (length a - 1)

search :: [Int] -> Int -> Int -> Int -> Int
search a k i j
  | i > j = -1
  | v < k = search a k (m+1) j
  | v > k = search a k i (m-1)
  | otherwise = m
  where m = div (i + j) 2
        v = a !! m

arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

main = do
  putStrLn $ show $ bsearch arr 2
  putStrLn $ show $ bsearch arr 4
  putStrLn $ show $ bsearch arr 8
  putStrLn $ show $ bsearch arr 10
