-- convert a number to it's english word format

teen_names = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]
ten_names = ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]
pows = [("Billion", 1000000000), ("Million", 1000000), ("Thousand", 1000), ("Hundred", 100)]

num2words :: Int -> String
num2words x
  | x == 0 = "Zero"
  | otherwise = unwords $ build x

build :: Int -> [String]
build x
  | x == 0 = []
  | (not . null) p = (build $ div x pp) ++ [pn] ++ (build $ mod x pp)
  | x >= 20 = [ten_names !! div x 10] ++ (build $ mod x 10)
  | otherwise = [teen_names !! x]
  where p = filter (\t -> x > (snd t)) pows
        pn = fst $ head p
        pp = snd $ head p

main = do
  putStrLn $ num2words 0
  putStrLn $ num2words 1
  putStrLn $ num2words 12
  putStrLn $ num2words 30
  putStrLn $ num2words 99
  putStrLn $ num2words 200
  putStrLn $ num2words 299
  putStrLn $ num2words 801
  putStrLn $ num2words 2001
  putStrLn $ num2words 7555
  putStrLn $ num2words 241090
  putStrLn $ num2words 201241090
  putStrLn $ num2words 201080241090
