-- count count of an element in a sorted array
-- using binary search variation

bcount :: [Int] -> Int -> Int
bcount a k
  | s < 0 = 0
  | otherwise = e - s + 1
  where s = start a k 0 (l-1)
        e =   end a k 0 (l-1)
        l = length a

start :: [Int] -> Int -> Int -> Int -> Int
start a k i j
  | i > j = -1
  | v < k = start a k (m+1) j
  | v > k = start a k i (m-1)
  | m == 0 || a !! (m-1) /= k = m
  | otherwise = start a k i (m-1)
  where m = div (i + j) 2
        v = a !! m

end :: [Int] -> Int -> Int -> Int -> Int
end a k i j
  | i > j = -1
  | v < k = end a k (m+1) j
  | v > k = end a k i (m-1)
  | m == length a - 1 || a !! (m+1) /= k = m
  | otherwise = end a k (m+1) j
  where m = div (i + j) 2
        v = a !! m


foo = [1, 1, 4, 4, 4, 4, 5, 6, 9, 9]
--     0  1  2  3  4  5  6  7  8  9

main = do
  putStrLn $ show $ bcount foo 1
  putStrLn $ show $ bcount foo 4
  putStrLn $ show $ bcount foo 5
  putStrLn $ show $ bcount foo 6
  putStrLn $ show $ bcount foo 9
  putStrLn $ show $ bcount foo 7
